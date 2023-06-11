const std = @import("std");
const Pg = @import("ParserGenerator.zig");
const Pattern = Pg.Pattern;
const Vm = @import("Vm.zig");
const memo = @import("memo.zig");
const isa = @import("isa.zig");
const input = @import("input.zig");

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
        const res = try code.exec(fbs.seekableStream(), &tbl);
        const match = res[0];
        const off = res[1];
        if (tt[1] == -1 and match or
            tt[1] != -1 and !match or
            tt[1] != -1 and tt[1] != off)
        {
            std.debug.print(
                "name='{s}' input='{s}' actual=({}, {}) expected=({})\n",
                .{ name, tt[0], match, off, tt[1] },
            );
            std.debug.print("match={} off={} tt.match={}", .{ match, off, tt[1] });
            return error.UnexpectedResult;
        }
    }
}

test "Concat" {
    const p = Pg.Group(&.{ Pg.String("ana"), Pg.String("hi") });

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
    const p = Pg.Check(Pg.OneOrMore(Pg.CharRange('0', '9')), &u8checker);

    const tests = .{
        .{ "123", 3 },
        .{ "256", -1 },
        .{ "foo", -1 },
        .{ "0", 1 },
    };

    try check(p, &tests);
}

test "Or" {
    const p = Pg.Select(&.{ Pg.String("ana"), Pg.String("hi") });

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
        const p = Pg.ZeroOrMore(Pg.String("ana"));
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
        const p = Pg.OneOrMore(Pg.String("hi"));
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
        const p = Pg.Group(&.{ Pg.OneOrMore(Pg.AnyOf("01")), Pg.ZeroOrMore(Pg.AnyOf("abc")) });
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
    const p = Pg.Negative(Pg.String("ana"));
    {
        const tests = .{
            .{ "ana", -1 },
            .{ "hi", 0 },
            .{ "an", 0 },
        };
        try check(p, &tests);
    }
    {
        const p1 = Pg.Negative(Pg.Negative(Pg.String("ana")));
        const p2 = Pg.Positive(Pg.String("ana"));
        const tests = .{
            .{ "ana", 0 },
            .{ "hi", -1 },
            .{ "an", -1 },
        };
        try check(p1, &tests);
        try check(p2, &tests);
    }
}

test "Any" {
    const p = Pg.Group(&.{ Pg.Any(5), Pg.String("ana") });
    const tests = .{
        .{ "helloana", 8 },
        .{ "hiana", -1 },
        .{ "anaanana", 8 },
    };
    try check(p, &tests);
}

test "Optional" {
    const p = Pg.Group(&.{ Pg.String("ana"), Pg.Optional(Pg.String("hello")) });
    const tests = .{
        .{ "ana", 3 },
        .{ "anahe", 3 },
        .{ "hello", -1 },
        .{ "anahello", 8 },
    };
    try check(p, &tests);
}

test "Set" {
    const p = Pg.OneOrMore(Pg.CharRange('0', '9'));
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

    const set = comptime Pg.initCharset("()");
    const S = Pg.Select(&.{
        Pg.NonTerm("B"),
        Pg.OneOrMore(Pg.Group(&.{ Pg.Negative(Pg.Set(set)), Pg.Any(1) })),
    });
    const B = Pg.Group(&.{
        Pg.Group(&.{ Pg.String("("), Pg.NonTerm("S") }),
        Pg.String(")"),
    });
    const p = try Pg.Pattern.rulesToGrammar(talloc, &.{
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
    const X = Pg.Select(&.{
        Pg.String("ana"),
        Pg.Group(&.{ Pg.Any(1), Pg.NonTerm("X") }),
    });
    const p = try Pg.Pattern.rulesToGrammar(talloc, &.{
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
    const p = Pg.OneOrMore(Pg.Select(&.{
        Pg.CharRange('a', 'z'),
        Pg.CharRange('A', 'Z'),
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
        const p = Pg.Search(Pg.Group(&.{Pg.String("ana")}));
        const tests = .{
            .{ "hello ana hello", 9 },
            .{ "hello", -1 },
            .{ "hello ana ana ana", 9 },
        };
        try check(p, &tests);
    }

    // search for last occurrence
    {
        const p = Pg.OneOrMore(Pg.Search(Pg.String("ana")));
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

    const Expr = Pg.Group(&.{
        Pg.NonTerm("Factor"),
        Pg.ZeroOrMore(Pg.Group(&.{ Pg.AnyOf(&.{ '+', '-' }), Pg.NonTerm("Factor") })),
    });
    const Factor = Pg.Group(&.{
        Pg.NonTerm("Term"),
        Pg.ZeroOrMore(Pg.Group(&.{ Pg.AnyOf(&.{ '*', '/' }), Pg.NonTerm("Term") })),
    });
    const Number = Pg.OneOrMore(Pg.CharRange('0', '9'));
    const Term = Pg.Select(&.{
        Pg.NonTerm("Number"),
        Pg.Group(&.{ Pg.Char('('), Pg.NonTerm("Expr"), Pg.Char(')') }),
    });
    const p = try Pg.Pattern.rulesToGrammar(talloc, &.{
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
    const word = Pg.OneOrMore(Pg.String("/"));
    var br = isa.BackReference{ .allocator = talloc };
    defer br.deinit();
    const p = Pg.GroupBuf(3, &.{
        Pg.CheckFlags(word, &br, 0, isa.Ref.code(.def)),
        Pg.ZeroOrMore(Pg.GroupBuf(2, &.{
            Pg.Negative(Pg.CheckFlags(.empty, &br, 0, isa.Ref.code(.use))),
            Pg.Any(1),
        })),
        Pg.CheckFlags(.empty, &br, 0, isa.Ref.code(.use)),
    });
    const tests = .{
        .{ "/// hello world ///", 19 },
        .{ "// hello world //", 17 },
        .{ "/// hello world //", -1 },
    };
    try check(p, &tests);
}
