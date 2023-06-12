const std = @import("std");
const pattern = @import("../pattern.zig");
const Pattern = pattern.Pattern;
const Vm = @import("../Vm.zig");
const memo = @import("../memo.zig");
const isa = @import("../isa.zig");
const input = @import("../input.zig");

const PatternTest = struct { []const u8, isize };

const testing = std.testing;
const talloc = testing.allocator;

fn check(p_: Pattern, tests: []const PatternTest) !void {
    var p = try p_.normalize(talloc);
    defer p.deinit(talloc);
    var prog = try p.compileAndOptimize(talloc);
    defer prog.deinit(talloc);
    var code = try Vm.encode(talloc, prog);
    defer code.deinit();

    for (tests, 1..) |tt, i| {
        const name = tt[0][0..@min(10, tt[0].len)];
        std.log.debug("=== RUN #{}: '{s}{s}'", .{ i, name, if (tt[0].len > 10) "..." else "" });
        var fbs = std.io.fixedBufferStream(tt[0]);
        var tbl = memo.Table{ .none = {} };
        // use an undefined captures arena as no captures are created
        const res = try code.exec(fbs.seekableStream(), &tbl, undefined);
        const match = res[0];
        const off = res[1];
        if (tt[1] == -1 and match or
            tt[1] != -1 and !match or
            tt[1] != -1 and tt[1] != off)
        {
            std.debug.print(
                "name='{s}' input='{s}' expected={} actual={}\n",
                .{ name, tt[0], tt[1], off },
            );
            return error.UnexpectedResult;
        }
    }
}

test "Concat" {
    const p = pattern.Group(&.{ pattern.String("ana"), pattern.String("hi") });

    const tests = .{
        .{ "ana", -1 },
        .{ "hi", -1 },
        .{ "anahi", 5 },
        .{ "anah", -1 },
    };
    try check(p, &tests);
}

// only allows integers between 0 and 256
const U8Checker = struct {
    pub fn check(_: ?*anyopaque, b: []const u8, _: ?*anyopaque, _: u32, _: u32) isize {
        const i = std.fmt.parseInt(usize, b, 10) catch return -1;
        return if (i >= 0 and i < 256)
            0
        else
            -1;
    }
};

test "Checker" {
    var u8checker = U8Checker{};
    const p = pattern.Check(pattern.OneOrMore(pattern.CharRange('0', '9')), &u8checker);

    const tests = .{
        .{ "123", 3 },
        .{ "256", -1 },
        .{ "foo", -1 },
        .{ "0", 1 },
    };

    try check(p, &tests);
}

test "Or" {
    const p = pattern.Select(&.{ pattern.String("ana"), pattern.String("hi") });

    const tests = .{
        .{ "ana", 3 },
        .{ "hi", 2 },
        .{ "an", -1 },
        .{ "anahi", 3 },
    };

    try check(p, &tests);
}

test "Repeat" {
    {
        const p = pattern.ZeroOrMore(pattern.String("ana"));
        const tests = .{
            .{ "", 0 },
            .{ "ana", 3 },
            .{ "anaanaana", 9 },
            .{ "hiana", 0 },
            .{ "anaanaan", 6 },
            .{ "an", 0 },
        };
        try check(p, &tests);
    }

    {
        const p = pattern.OneOrMore(pattern.String("hi"));
        const tests = .{
            .{ "", -1 },
            .{ "hi", 2 },
            .{ "hihihi", 6 },
            .{ "hihiana", 4 },
            .{ "h", -1 },
        };
        try check(p, &tests);
    }

    {
        const p = pattern.Group(&.{ pattern.OneOrMore(pattern.AnyOf("01")), pattern.ZeroOrMore(pattern.AnyOf("abc")) });
        const tests = .{
            .{ "01", 2 },
            .{ "01abaabbc", 9 },
            .{ "abc", -1 },
            .{ "5a", -1 },
            .{ "1z", 1 },
        };
        try check(p, &tests);
    }
}

test "Predicate" {
    const p = pattern.Negative(pattern.String("ana"));
    {
        const tests = .{
            .{ "ana", -1 },
            .{ "hi", 0 },
            .{ "an", 0 },
        };
        try check(p, &tests);
    }
    {
        const p1 = pattern.Negative(pattern.Negative(pattern.String("ana")));
        const p2 = pattern.Positive(pattern.String("ana"));
        const tests = .{
            .{ "ana", 0 },
            .{ "hi", -1 },
            .{ "an", -1 },
        };
        try check(p1, &tests);
        try check(p2, &tests);
    }
}

test "Not" {
    {
        const p = pattern.OneOrMore(pattern.Not(pattern.CharRange('a', 'z')));
        const tests = .{
            .{ "ana", -1 },
            .{ "12", 2 },
            .{ "1a", 1 },
        };
        try check(p, &tests);
    }
    {
        const p = pattern.OneOrMore(pattern.Not(pattern.Not(pattern.CharRange('a', 'z'))));
        const tests = .{
            .{ "ana", 3 },
            .{ "12", -1 },
            .{ "1a", -1 },
            .{ "a1", 1 },
        };
        try check(p, &tests);
    }
}

test "Any" {
    const p = pattern.Group(&.{ pattern.Any(5), pattern.String("ana") });
    const tests = .{
        .{ "helloana", 8 },
        .{ "hiana", -1 },
        .{ "anaanana", 8 },
    };
    try check(p, &tests);
}

test "Optional" {
    const p = pattern.Group(&.{ pattern.String("ana"), pattern.Optional(pattern.String("hello")) });
    const tests = .{
        .{ "ana", 3 },
        .{ "anahe", 3 },
        .{ "hello", -1 },
        .{ "anahello", 8 },
    };
    try check(p, &tests);
}

test "Set" {
    const p = pattern.OneOrMore(pattern.CharRange('0', '9'));
    const tests = .{
        .{ "hi", -1 },
        .{ "1002", 4 },
        .{ "10.02", 2 },
        .{ "9", 1 },
    };
    try check(p, &tests);
}

test "Grammar" {
    // grammar:
    // S <- <B> / (![()] .)+
    // B <- '(' <S> ')'

    const set = comptime pattern.initCharset("()");
    const S = pattern.Select(&.{
        pattern.NonTerm("B"),
        pattern.OneOrMore(pattern.Group(&.{ pattern.Negative(pattern.Set(set)), pattern.Any(1) })),
    });
    const B = pattern.Group(&.{
        pattern.Group(&.{ pattern.String("("), pattern.NonTerm("S") }),
        pattern.String(")"),
    });
    const p = try pattern.Pattern.rulesToGrammar(talloc, &.{
        .{ "S", S },
        .{ "B", B },
    }, "S");
    const tests = .{
        .{ "(hello)", 7 },
        .{ "(hello", -1 },
        .{ "((inside))", 10 },
        .{ "((inside)", -1 },
    };
    try check(p, &tests);
}

test "TailCall" {
    const X = pattern.Select(&.{
        pattern.String("ana"),
        pattern.Group(&.{ pattern.Any(1), pattern.NonTerm("X") }),
    });
    const p = try pattern.Pattern.rulesToGrammar(talloc, &.{
        .{ "X", X },
    }, "X");
    const tests = .{
        .{ "asdf", -1 },
        .{ "ana hello", 3 },
        .{ "hello ana", 9 },
        .{ "anaana", 3 },
    };
    try check(p, &tests);
}

test "UnionSet" {
    const p = pattern.OneOrMore(pattern.Select(&.{
        pattern.CharRange('a', 'z'),
        pattern.CharRange('A', 'Z'),
    }));
    const tests = .{
        .{ "Hello", 5 },
        .{ "123", -1 },
        .{ "Hello1", 5 },
    };
    try check(p, &tests);
}

test "Search" {
    {
        const p = pattern.Search(pattern.Group(&.{pattern.String("ana")}));
        const tests = .{
            .{ "hello ana hello", 9 },
            .{ "hello", -1 },
            .{ "hello ana ana ana", 9 },
        };
        try check(p, &tests);
    }

    // search for last occurrence
    {
        const p = pattern.OneOrMore(pattern.Search(pattern.String("ana")));
        const tests = .{
            .{ "hello ana hello", 9 },
            .{ "hello", -1 },
            .{ "hello ana ana ana hello", 17 },
        };
        try check(p, &tests);
    }
}

test "ArithmeticGrammar" {
    // grammar:
    // Expr   <- <Factor> ([+-] <Factor>)*
    // Factor <- <Term> ([*/] <Term>)*
    // Term   <- <Number> / '(' <Expr> ')'
    // Number <- [0-9]+

    const Expr = pattern.Group(&.{
        pattern.NonTerm("Factor"),
        pattern.ZeroOrMore(pattern.Group(&.{ pattern.AnyOf(&.{ '+', '-' }), pattern.NonTerm("Factor") })),
    });
    const Factor = pattern.Group(&.{
        pattern.NonTerm("Term"),
        pattern.ZeroOrMore(pattern.Group(&.{ pattern.AnyOf(&.{ '*', '/' }), pattern.NonTerm("Term") })),
    });
    const Number = pattern.OneOrMore(pattern.CharRange('0', '9'));
    const Term = pattern.Select(&.{
        pattern.NonTerm("Number"),
        pattern.Group(&.{ pattern.Char('('), pattern.NonTerm("Expr"), pattern.Char(')') }),
    });
    const p = try pattern.Pattern.rulesToGrammar(talloc, &.{
        .{ "Expr", Expr },
        .{ "Factor", Factor },
        .{ "Term", Term },
        .{ "Number", Number },
    }, "Expr");
    const tests = .{
        .{ "13+(22-15)", 10 },
        .{ "24*5+3", 6 },
        .{ "word 5*3", -1 },
        .{ "10*(43", 2 },
    };
    try check(p, &tests);
}

test "BackReference" {
    const word = pattern.OneOrMore(pattern.String("/"));
    var br = isa.BackReference{ .allocator = talloc };
    defer br.deinit();
    const p = pattern.GroupBuf(3, &.{
        pattern.CheckFlags(word, &br, 0, isa.Ref.code(.def)),
        pattern.ZeroOrMore(pattern.GroupBuf(2, &.{
            pattern.Negative(pattern.CheckFlags(.empty, &br, 0, isa.Ref.code(.use))),
            pattern.Any(1),
        })),
        pattern.CheckFlags(.empty, &br, 0, isa.Ref.code(.use)),
    });
    const tests = .{
        .{ "/// hello world ///", 19 },
        .{ "// hello world //", 17 },
        .{ "/// hello world //", -1 },
    };
    try check(p, &tests);
}
