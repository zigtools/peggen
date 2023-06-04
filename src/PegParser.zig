const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const testing = std.testing;
const Stream = @import("Stream.zig");
const Pg = @import("ParserGenerator.zig");

const PegParser = @This();

allocator: mem.Allocator,
stream: Stream,
ctx: Pg.Context,

pub fn init(allocator: mem.Allocator, contents: []const u8, output: []u8, file_path: []const u8) PegParser {
    return .{
        .allocator = allocator,
        .stream = Stream.init(contents, output),
        .ctx = .{ .file_path = file_path },
    };
}

pub const Expression = struct {
    pub const Set = struct {
        pub const Kind = enum { positive, negative };

        kind: Kind = .positive,
        values: []const u8,
    };

    pub const Body = union(enum) {
        /// .
        any,
        identifier: []const u8,
        /// "characters" 'characters'
        string: []const u8,
        /// [a-zA-Z_] [^0-9]
        set: Set,
        /// (abc def)
        group: std.ArrayListUnmanaged(Expression),
        /// abc / def / gej
        select: std.ArrayListUnmanaged(Expression),

        pub const Tag = std.meta.Tag(Body);
    };

    pub const Modifier = enum {
        none,
        optional,
        zero_or_more,
        one_or_more,
    };

    pub const Lookahead = enum {
        none,
        positive,
        negative,
    };

    lookahead: Lookahead = .none,
    body: Body,
    modifier: Modifier = .none,

    pub fn deinit(e: *Expression, allocator: mem.Allocator) void {
        switch (e.body) {
            .group, .select => |*l| {
                for (l.items) |*sube| sube.deinit(allocator);
                l.deinit(allocator);
            },
            else => {},
        }
    }

    pub fn format(e: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try formatImpl(e, fmt, options, writer, 0);
    }

    pub fn formatImpl(e: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype, depth: u8) !void {
        const print_parens =
            ((e.modifier != .none and e.body == .group and e.body.group.items.len > 1) or
            (depth != 0 and e.body == .select and e.body.select.items.len > 1));
        try writer.writeAll(switch (e.lookahead) {
            .negative => "!",
            .positive => "&",
            .none => "",
        });
        if (print_parens) _ = try writer.write("(");
        switch (e.body) {
            .any => try writer.writeByte('.'),
            .identifier => |s| _ = try writer.write(s),
            .string => |s| {
                assert(s.len > 1);
                if (s[0] == '\'')
                    try writer.print("'{'}'", .{std.fmt.Formatter(formatEscapes){
                        .data = s[1 .. s.len - 1],
                    }})
                else if (s[0] == '"')
                    try writer.print("\"{}\"", .{std.fmt.Formatter(formatEscapes){
                        .data = s[1 .. s.len - 1],
                    }})
                else
                    unreachable;
            },
            .set => |s| {
                try writer.print("[{set}]", .{std.fmt.Formatter(formatEscapes){
                    .data = s.values[1 .. s.values.len - 1],
                }});
            },
            .group => |g| for (g.items, 0..) |item, i| {
                if (i != 0) _ = try writer.write(" ");
                try formatImpl(item, fmt, options, writer, depth + 1);
            },
            // print top level select items on separate lines
            .select => |g| for (g.items, 0..) |item, i| {
                // add newline and aligned spacing when depth == 0
                if (depth == 0) {
                    _ = try writer.write("\n");
                    if (i == 0) {
                        try writer.writeByteNTimes(' ', 6);
                    } else {
                        try writer.writeByteNTimes(' ', 4);
                    }
                }
                // print aligned '/' separator
                if (i != 0) {
                    if (depth == 0)
                        _ = try writer.write("/ ")
                    else
                        _ = try writer.write(" / ");
                }
                try formatImpl(item, fmt, options, writer, depth + 1);
            },
        }
        if (print_parens) _ = try writer.write(")");
        _ = try writer.write(switch (e.modifier) {
            .none => "",
            .zero_or_more => "*",
            .one_or_more => "+",
            .optional => "?",
        });
    }

    pub const Formatter = struct {
        expr: Expression,
        rule: []const u8,

        pub fn format(formatter: Formatter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = options;
            _ = fmt;

            const expr = formatter.expr;
            const rule = formatter.rule;

            switch (expr.lookahead) {
                .none => {},
                .positive => try writer.print("ParserGenerator.Positive(.{}, ", .{std.zig.fmtId(rule)}),
                .negative => try writer.print("ParserGenerator.Negative(.{}, ", .{std.zig.fmtId(rule)}),
            }

            switch (expr.modifier) {
                .none => {},
                .optional => try writer.print("ParserGenerator.Optional(.{}, ", .{std.zig.fmtId(rule)}),
                .zero_or_more => try writer.print("ParserGenerator.ZeroOrMore(.{}, ", .{std.zig.fmtId(rule)}),
                .one_or_more => try writer.print("ParserGenerator.OneOrMore(.{}, ", .{std.zig.fmtId(rule)}),
            }

            switch (expr.body) {
                .any => try writer.print("ParserGenerator.Any(.{})", .{std.zig.fmtId(rule)}),
                .identifier => |id| try writer.print("{}", .{std.zig.fmtId(id)}),
                .string => |str| {
                    assert(str.len > 2);
                    if (str[0] == '\'') {
                        if (str.len == 3)
                            try writer.print(
                                "ParserGenerator.Char(.{}, '{'}')",
                                .{ std.zig.fmtId(rule), std.zig.fmtEscapes(&.{str[1]}) },
                            )
                        else
                            try writer.print(
                                "ParserGenerator.String(.{}, \"{'}\")",
                                .{ std.zig.fmtId(rule), std.zig.fmtEscapes(str[1 .. str.len - 1]) },
                            );
                    } else if (str[0] == '"') {
                        if (str.len == 3)
                            try writer.print(
                                "ParserGenerator.Char(.{}, \"{}\")",
                                .{ std.zig.fmtId(rule), std.zig.fmtEscapes(&.{str[1]}) },
                            )
                        else
                            try writer.print(
                                "ParserGenerator.String(.{}, \"{}\")",
                                .{ std.zig.fmtId(rule), std.zig.fmtEscapes(str[1 .. str.len - 1]) },
                            );
                    } else {
                        std.log.err("invalid string starting character '{c}'. expected ' or \".", .{str[0]});
                        return;
                    }
                },
                .set => |set| {
                    var buf: [16]u8 = undefined;
                    // trim off leading '[^' and trailing ']'
                    const input =
                        set.values[1 + @as(u8, @boolToInt(set.kind == .negative)) .. set.values.len - 1];
                    var stream = Stream.init(input, &buf);
                    var ctx = Pg.Context{ .file_path = "<set range ctx>" };

                    // need to separate the range by '-' but not '\-' and the
                    // 'range' parser doesn't accept non-printable ascii
                    // characters.
                    // this quick and dirty - maybe hacky - escaper does the job.
                    // it just replaces '\-' w/ '-'
                    const any_char0 = Pg.CharRange(.any_char, 0, 255);
                    const any_char = Pg.Select(.any_char, .{
                        Pg.Escape(
                            .escape_any_char,
                            Pg.Group(.bslash_any_char, .{ backslash, any_char0 }),
                            error{NoSpaceLeft},
                            struct {
                                fn func(slice: []const u8, s: *Stream) !void {
                                    if (slice.len == 2 and slice[0] == '\\' and slice[1] == '-') {
                                        try s.writeByte('-');
                                    } else try s.writeByte(slice[0]);
                                }
                            }.func,
                        ),
                        any_char0,
                    });

                    const ac_dash_ac = Pg.Group(.ac_dash_ac, .{ any_char, dash, any_char });
                    const range_any = Pg.Select(.range_any, .{ ac_dash_ac, any_char });
                    var parser_iter = Pg.iterator(range_any, &stream, &ctx);

                    const count = parser_iter.count() catch |e| {
                        std.log.err("{} during parser_iter.count() input={s}", .{ e, input });
                        return;
                    };
                    parser_iter.reset();

                    if (count > 1)
                        try writer.print("ParserGenerator.Select(.{}, .{{", .{std.zig.fmtId(rule)});

                    while (true) {
                        const raw_range = parser_iter.next() catch |e| {
                            std.log.err(
                                "{}. could not parse range '{s}' from square set '{s}'",
                                .{ e, stream.input[stream.index..], set.values },
                            );
                            return;
                        } orelse break;

                        if (set.kind == .negative) {
                            try writer.print("ParserGenerator.Not(.{}, ", .{std.zig.fmtId(rule)});
                        }

                        if (raw_range.len == 1) {
                            // single char
                            try writer.print(
                                "ParserGenerator.Char(.{}, '{'}')",
                                .{ std.zig.fmtId(rule), std.zig.fmtEscapes(&.{raw_range[0]}) },
                            );
                        } else {
                            // must be either range of 2 chars or escaped dash.
                            const first_second: [2][]const u8 = switch (raw_range.len) {
                                2 => blk: {
                                    assert(raw_range[0] == '\\');
                                    break :blk .{ raw_range[1..], "" };
                                },
                                3 => blk: {
                                    assert(raw_range[1] == '-');
                                    break :blk .{ raw_range[0..1], raw_range[2..3] };
                                },
                                else => unreachable,
                            };
                            const first = first_second[0];
                            const second = first_second[1];
                            assert(first.len > 0);
                            std.log.info("set.kind={} values={s} first={s} second={s} raw_range='{s}'", .{ set.kind, set.values, first, second, raw_range });
                            if (second.len == 0) {
                                try writer.print(
                                    "ParserGenerator.Char(.{}, '{'}')",
                                    .{ std.zig.fmtId(rule), std.zig.fmtEscapes(&.{first[0]}) },
                                );
                            } else {
                                assert(second.len == 1);
                                try writer.print(
                                    "ParserGenerator.CharRange(.{}, '{'}', '{'}')",
                                    .{
                                        std.zig.fmtId(rule),
                                        std.zig.fmtEscapes(&.{first[0]}),
                                        std.zig.fmtEscapes(&.{second[0]}),
                                    },
                                );
                            }
                        }

                        if (set.kind == .negative) try writer.writeAll(")");
                        if (count > 1) try writer.writeByte(',');
                    }
                    if (count > 1)
                        try writer.writeAll("})");
                },
                .group => |group| {
                    try writer.print("ParserGenerator.Group(.{}, .{{", .{std.zig.fmtId(rule)});
                    for (group.items) |sub_expr| {
                        try writer.print("{},", .{Formatter{ .expr = sub_expr, .rule = rule }});
                    }
                    try writer.writeAll("})");
                },
                .select => |select| {
                    try writer.print("ParserGenerator.Select(.{}, .{{", .{std.zig.fmtId(rule)});
                    for (select.items) |sub_expr| {
                        try writer.print("{},", .{Formatter{ .expr = sub_expr, .rule = rule }});
                    }
                    try writer.writeAll("})");
                },
            }

            switch (expr.modifier) {
                .none => {},
                else => try writer.writeAll(")"),
            }

            switch (expr.lookahead) {
                .none => {},
                else => try writer.writeAll(")"),
            }
        }
    };
};

test "Expression.format" {
    {
        const input =
            \\"'"
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const expr = try p.parsePrimary();
        var buf: [10]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{}", .{expr}));
    }
    {
        const input =
            \\'"'
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const expr = try p.parsePrimary();
        var buf: [10]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{}", .{expr}));
    }
    {
        const input =
            \\[\200-\277]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const expr = try p.parsePrimary();
        var buf: [20]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{}", .{expr}));
    }
    {
        const input =
            \\'\364'
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const expr = try p.parsePrimary();
        var buf: [10]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{}", .{expr}));
    }
    {
        const input =
            \\"\\\\"
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const expr = try p.parsePrimary();
        var buf: [10]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{}", .{expr}));
    }
    {
        const input =
            \\[nr\\t'"]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const expr = try p.parsePrimary();
        var buf: [10]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{}", .{expr}));
    }
}

pub const Rule = struct {
    identifier: []const u8,
    expression: Expression,
};

pub const Grammar = struct {
    rules: std.ArrayListUnmanaged(Rule) = .{},

    pub fn deinit(g: *Grammar, allocator: mem.Allocator) void {
        for (g.rules.items) |*r| r.expression.deinit(allocator);
        g.rules.deinit(allocator);
    }
};

fn runParser(self: *PegParser, comptime parser: anytype) ![]const u8 {
    return try Pg.parse(parser, &self.stream, &self.ctx);
}

const alpha = Pg.CharFn(.alpha, std.ascii.isAlphabetic);
const alpha_num = Pg.CharFn(.alpha, std.ascii.isAlphanumeric);
const ident_others = Pg.Select(.ident_others, .{Pg.Char(.uscore, '_')});
const ident_succ = Pg.Select(.identifier, .{ alpha_num, ident_others });
const ident = Pg.Group(.ident, .{
    alpha,
    Pg.ZeroOrMore(.many_ident_succ, ident_succ),
    spacing,
});

const leftarrow = Pg.Group(.leftarrow, .{ Pg.String(.leftarrow_str, "<-"), spacing });
const slash = Pg.Group(.slash, .{ Pg.Char(.slash_char, '/'), spacing });

/// Spacing   <- (Space / Comment)*
/// Comment   <- '#' (!EndOfLine .)* EndOfLine
/// Space   <- ' ' / '\t' / EndOfLine
/// EndOfLine <- '\r\n' / '\n' / '\r'
const ws = Pg.CharFn(.ws, std.ascii.isWhitespace);
const wss = Pg.ZeroOrMore(.wss, ws);

const space_or_tab = Pg.AnyOf(.space_or_tab, " \t");
const nl_or_lf = Pg.AnyOf(.nl_or_lf, "\n\r");
const end_of_line = Pg.Select(.end_of_line, .{ Pg.String(.eol_rn, "\r\n"), nl_or_lf });
const space = Pg.Select(.space, .{ space_or_tab, end_of_line });
const hash = Pg.Char(.hash, '#');
const comment = Pg.Group(.comment_group, .{
    Pg.Positive(.hash_look, hash),
    Pg.ZeroOrMore(
        .not_eol_star,
        Pg.Group(.not_eol_any, .{ Pg.Negative(.not_eol, end_of_line), Pg.Any(.not_eol_any) }),
    ),
    end_of_line,
});
pub const spacing = Pg.ZeroOrMore(.spacing, Pg.Select(.space_or_comment, .{ space, comment }));

/// Print the string as escaped contents of a square set, double-quoted string
/// or single-quoted string.
/// fmt 'set' indicates square set escaping
/// fmt '' indicates double-quoted escaping
/// fmt "'" indicates single-quoted escaping
fn formatEscapes(
    bytes: []const u8,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;
    const is_set = mem.eql(u8, fmt, "set");
    const is_single = fmt.len == 1 and fmt[0] == '\'';
    const is_double = fmt.len == 0;

    var i: usize = 0;

    while (i < bytes.len) : (i += 1) {
        const byte = bytes[i];
        switch (byte) {
            0x07 => try writer.writeAll("\\a"),
            0x08 => try writer.writeAll("\\b"),
            0x1B => try writer.writeAll("\\e"),
            0x0C => try writer.writeAll("\\f"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x0B => try writer.writeAll("\\v"),
            '[' => try writer.writeAll(if (is_set) "\\[" else "["),
            ']' => try writer.writeAll(if (is_set) "\\]" else "]"),
            '\'' => try writer.writeAll(if (is_single) "\\'" else "'"),
            '"' => try writer.writeAll(if (is_double) "\\\"" else "\""),
            // prevent double escaping escaped dash in square sets
            '\\' => if (is_set and i + 1 < bytes.len and bytes[i + 1] == '-') {
                try writer.writeAll("\\-");
                i += 1;
            } else try writer.writeAll("\\\\"),
            ' ',
            '!',
            '#'...'&',
            '('...'[' - 1,
            ']' + 1...'~',
            => try writer.writeByte(byte),
            else => {
                // try writer.print("{}", .{byte});
                try writer.writeByte('\\');
                try std.fmt.formatInt(byte, 8, .lower, .{ .width = 2, .fill = '0' }, writer);
            },
        }
    }
}

pub fn parseEscapeSequence(slice: []const u8, stream: *Stream) !void {
    if (slice.len < 2) return error.InvalidEscape;
    assert(slice[0] == '\\');

    const output: []const u8 = switch (slice[1]) {
        'a' => &.{0x07},
        'b' => &.{0x08},
        'e' => &.{0x1B},
        'f' => &.{0x0C},
        'n' => "\n",
        'r' => "\r",
        't' => "\t",
        'v' => &.{0x0B},
        '\'' => "\'",
        '"' => "\"",
        '[' => "[",
        ']' => "]",
        '\\' => "\\",
        '-' => "\\-",

        '0'...'7' => blk: {
            const len = for (slice[2..], 2..) |c, i| {
                if (!('0' <= c and c <= '7')) break i;
            } else slice.len;
            const octstr = slice[1..len];
            const oct = try std.fmt.parseUnsigned(u8, octstr, 8);
            break :blk &.{oct};
        },
        else => return error.InvalidEscape,
    };
    _ = try stream.write(output);
}

/// Char    <- '\\' [abefnrtv'"\[\]\\]
///          / '\\' [0-3][0-7][0-7]
///          / '\\' [0-7][0-7]?
///          / '\\' '-'
///          / !'\\' .
const dot = Pg.Group(.dot, .{ Pg.Char(.dot_char, '.'), spacing });
const backslash = Pg.Char(.backslash, '\\');
const escapees = Pg.AnyOf(.escapees,
    \\abefnrtv'"[]\
);
const zero_to_three = Pg.CharRange(.zero_to_three, '0', '3');
const zero_to_seven = Pg.CharRange(.zero_to_seven, '0', '7');
const char = Pg.Select(.char, .{
    Pg.Escape(
        .escape_char,
        Pg.Select(
            .escaped_char,
            .{
                Pg.Group(.char1, .{ backslash, escapees }),
                Pg.Group(.char2, .{ backslash, zero_to_three, zero_to_seven, zero_to_seven }),
                Pg.Group(.char3, .{ backslash, zero_to_seven, Pg.Optional(.char3_1, zero_to_seven) }),
                Pg.Group(.char4, .{ backslash, dash }),
            },
        ),
        error{ Overflow, InvalidCharacter, InvalidEscape, NoSpaceLeft },
        parseEscapeSequence,
    ),
    Pg.Group(.char5, .{ Pg.Negative(.char5_1, backslash), dot }),
    Pg.CharFn(.alphanum, std.ascii.isPrint),
});

/// Range   <- Char '-' Char / Char
const dash = Pg.Char(.dash, '-');
const range1 = Pg.Group(.char_dash_char, .{ char, dash, char });
const range = Pg.Select(.range, .{ range1, char });
const lbrace = Pg.Char(.lbrace, '[');
const rbrace = Pg.Char(.rbrace, ']');
/// Class <- '[' (!']' Range)* ']' Spacing
pub fn parseClass(self: *PegParser) ![]const u8 {
    const class = Pg.Group(.class, .{
        lbrace,
        Pg.ZeroOrMore(.class1, Pg.Group(.class1_1, .{
            Pg.Negative(.not_rbrace, rbrace),
            range,
        })),
        rbrace,
        spacing,
    });
    return try self.runParser(class);
}

test parseClass {
    {
        const input = "[a-zA-Z_]";
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const r = try p.parseClass();
        try testing.expectEqualStrings(input, r);
    }
    {
        const input = "[ab-z]";
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const r = try p.parseClass();
        try testing.expectEqualStrings(input, r);
        var stream = Stream.init(input[1 .. input.len - 1], &output);
        var ctx = Pg.Context{ .file_path = "<test>" };
        var iter = Pg.iterator(range, &stream, &ctx);
        try testing.expectEqualStrings("a", (try iter.next()).?);
        try testing.expectEqualStrings("b-z", (try iter.next()).?);
    }
    {
        const input =
            \\[abefnrtv'"\[\]\\]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const r = try p.parseClass();
        try testing.expectEqualStrings(
            \\[abefnrtv'"[]\]
        , r);
    }
}

const single_quote = Pg.Char(.single_quote, '\'');
const double_quote = Pg.Char(.double_quote, '"');
const lit_single = Pg.Group(
    .lit_single,
    .{ single_quote, Pg.ZeroOrMore(.lit1_1, Pg.Group(.lit1_2, .{
        Pg.Negative(.lit1_3, single_quote),
        char,
    })), single_quote, spacing },
);
const lit_double = Pg.Group(
    .lit_double,
    .{ double_quote, Pg.ZeroOrMore(.lit2_1, Pg.Group(.lit2_2, .{
        Pg.Negative(.lit2_3, double_quote),
        char,
    })), double_quote, spacing },
);

/// Literal   <- ['] (!['] Char )* ['] Spacing
///            / ["] (!["] Char )* ["] Spacing
pub fn parseLiteral(self: *PegParser) ![]const u8 {
    const literal = Pg.Select(.literal, .{ lit_single, lit_double });
    return try self.runParser(literal);
}

const lparen = Pg.Char(.lparen, '(');
const rparen = Pg.Char(.rparen, ')');
const open = Pg.Group(.open, .{ lparen, spacing });
const close = Pg.Group(.close, .{ rparen, spacing });

/// Primary   <- Identifier !LEFTARROW
///      / OPEN Expression CLOSE
///      / Literal
///      / Class
///      / DOT
pub fn parsePrimary(self: *PegParser) anyerror!Expression {
    const p1 = Pg.Group(.primary1, .{ ident, leftarrow });
    if (self.runParser(Pg.Positive(.peek_leftarrow, p1))) |_|
        return error.RuleEnd
    else |_| {}

    if (self.runParser(ident)) |identifier| {
        std.log.debug("parsePrimary() identifier={s}", .{identifier});
        return .{ .body = .{ .identifier = identifier } };
    } else |_| if (self.runParser(open)) |_| {
        // OPEN Expression CLOSE
        const index = self.stream.index;
        errdefer self.stream.index = index;
        const expr = try self.parseExpression();
        _ = try self.runParser(close);
        std.log.debug("parsePrimary() parens expression={}", .{expr});
        return expr;
    } else |_| if (self.parseClass()) |s| {
        std.log.debug("parsePrimary() set={s}", .{s});
        const kind: Expression.Set.Kind = if (s[1] == '^')
            .negative
        else
            .positive;
        return .{ .body = .{ .set = .{ .values = s, .kind = kind } } };
    } else |_| if (self.parseLiteral()) |s| {
        std.log.debug("parsePrimary() literal={s}", .{s});
        return .{ .body = .{ .string = s } };
    } else |_| if (self.runParser(dot)) |_| {
        std.log.debug("parsePrimary() dot", .{});
        return .{ .body = .any };
    } else |e| return e;
}

test parsePrimary {
    {
        const input =
            \\EndOfFile
        ;
        var output: [input.len]u8 = undefined;
        var p = init(testing.allocator, input, &output, "<test>");
        const i = try p.runParser(ident);
        try testing.expectEqualStrings("EndOfFile", i);
    }
    {
        const input =
            \\[a-z]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(testing.allocator, input, &output, "<test>");
        const e = try p.parsePrimary();
        try testing.expect(e.body == .set);
        try testing.expect(e.body.set.kind == .positive);
    }
    {
        const input =
            \\[^a-z]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(testing.allocator, input, &output, "<test>");
        const e = try p.parsePrimary();
        try testing.expect(e.body == .set);
        try testing.expect(e.body.set.kind == .negative);
        var buf: [10]u8 = undefined;
        try testing.expectEqualStrings("[^a-z]", try std.fmt.bufPrint(&buf, "{}", .{e}));
    }
    {
        const input =
            \\[+\-]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const r = try p.parsePrimary();
        try testing.expectEqualStrings(input, r.body.set.values);
        var buf: [10]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{set}", .{r}));
    }
    {
        const input =
            \\[\200-\277]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(undefined, input, &output, "<test>");
        const e = try p.parsePrimary();
        var buf: [20]u8 = undefined;
        try testing.expectEqualStrings(input, try std.fmt.bufPrint(&buf, "{}", .{e}));
    }
}

/// Suffix <- Primary (QUESTION / STAR / PLUS)?
pub fn parseSuffix(self: *PegParser) !Expression {
    var primary = try self.parsePrimary();
    const suffix_cont = Pg.Optional(.suffix_cont, Pg.Select(.suffix_cont_char, .{
        Pg.Group(.question, .{ Pg.Char(.question_char, '?'), spacing }),
        Pg.Group(.star, .{ Pg.Char(.star_char, '*'), spacing }),
        Pg.Group(.plus, .{ Pg.Char(.plus_char, '+'), spacing }),
    }));
    const cont = self.runParser(suffix_cont) catch unreachable;
    if (cont.len > 0) switch (cont[0]) {
        '?' => primary.modifier = .optional,
        '*' => primary.modifier = .zero_or_more,
        '+' => primary.modifier = .one_or_more,
        else => unreachable,
    };
    return primary;
}

const and_ = Pg.Group(.@"and", .{ Pg.Char(.and_char, '&'), spacing });
const not = Pg.Group(.not, .{ Pg.Char(.not_char, '!'), spacing });
/// Prefix <- AND Suffix / NOT Suffix / Suffix
pub fn parsePrefix(self: *PegParser) !Expression {
    const lookahead: Expression.Lookahead = if (self.runParser(and_)) |_|
        .positive
    else |_| if (self.runParser(not)) |_|
        .negative
    else |_|
        .none;

    var suffix = try self.parseSuffix();
    suffix.lookahead = lookahead;
    return suffix;
}

/// Sequence  <- Prefix (Prefix)* /
pub fn parseSequence(self: *PegParser) !Expression {
    var result = std.ArrayListUnmanaged(Expression){};
    const first_prefix = try self.parsePrefix();
    std.log.debug("parseSequence() first_prefix={}", .{first_prefix});
    while (true) {
        const prefix = self.parsePrefix() catch |e| switch (e) {
            error.ParseFailure, error.RuleEnd => break,
            else => return e,
        };
        std.log.debug("parseSequence() prefix={}", .{prefix});
        if (result.items.len == 0) {
            try result.append(self.allocator, first_prefix);
        }
        try result.append(self.allocator, prefix);
    }
    return if (result.items.len == 0)
        first_prefix
    else
        Expression{ .body = .{ .group = result } };
}

/// Expression  <- Sequence (SLASH Sequence)*
pub fn parseExpression(self: *PegParser) !Expression {
    var result = std.ArrayListUnmanaged(Expression){};
    const first_seq = try self.parseSequence();
    std.log.debug("parseExpression() first_seq={}", .{first_seq});

    while (true) {
        _ = self.runParser(slash) catch break;
        std.log.debug("slash", .{});
        const seq2 = self.parseSequence() catch |e| switch (e) {
            error.EndOfStream, error.RuleEnd => break,
            else => return e,
        };
        std.log.debug("parseExpression() seq2={}", .{seq2});
        if (result.items.len == 0) {
            try result.append(self.allocator, first_seq);
        }
        try result.append(self.allocator, seq2);
    }
    std.log.debug("parseExpression() done first_seq={} result={any}", .{ first_seq, result.items });

    return if (result.items.len == 0)
        first_seq
    else
        Expression{ .body = .{ .select = result } };
}

test parseExpression {
    const input =
        \\A+ B
    ;
    var output: [input.len]u8 = undefined;
    var p = init(testing.allocator, input, &output, "<test>");
    var e = try p.parseExpression();
    defer e.deinit(testing.allocator);
    try testing.expectEqual(Expression.Body.Tag.group, e.body);
    try testing.expectEqual(@as(usize, 2), e.body.group.items.len);
}

/// Definition  <- Identifier LEFTARROW Expression
pub fn parseDefinition(self: *PegParser) !Rule {
    const identifier = try self.runParser(ident);
    std.log.debug("parseDefinition() identifier={s}", .{identifier});
    _ = try self.runParser(leftarrow);
    std.log.debug("parseDefinition() leftarrow", .{});
    const expression = try self.parseExpression();
    std.log.debug("parseDefinition() expression={}", .{expression});
    return .{ .identifier = identifier, .expression = expression };
}

test parseDefinition {
    {
        const input =
            \\Grammar   <- Spacing Definition+ EndOfFile
        ;
        var output: [input.len]u8 = undefined;
        var p = init(testing.allocator, input, &output, "<test>");
        var r = try p.parseDefinition();
        try testing.expectEqualStrings("Grammar", r.identifier);
        defer r.expression.deinit(testing.allocator);
        try testing.expectEqual(Expression.Body.Tag.group, r.expression.body);
        try testing.expectEqual(@as(usize, 3), r.expression.body.group.items.len);
    }
    {
        const input =
            \\Char    <- '\\' [abefnrtv'"\[\]\\]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(testing.allocator, input, &output, "<test>");
        var r = try p.parseDefinition();
        defer r.expression.deinit(testing.allocator);
        try testing.expectEqualStrings("Char", r.identifier);
        try testing.expectEqual(Expression.Body.Tag.group, r.expression.body);
        try testing.expectEqual(@as(usize, 2), r.expression.body.group.items.len);
        try testing.expectEqual(Expression.Body.Tag.string, r.expression.body.group.items[0].body);
        try testing.expectEqual(Expression.Body.Tag.set, r.expression.body.group.items[1].body);
    }
    {
        const input =
            \\Char    <- '\\' [abefnrtv'"\[\]\\]
            \\         / '\\' [0-3][0-7][0-7]
        ;
        var output: [input.len]u8 = undefined;
        var p = init(testing.allocator, input, &output, "<test>");
        var r = try p.parseDefinition();
        defer r.expression.deinit(testing.allocator);
        try testing.expectEqualStrings("Char", r.identifier);
        // std.debug.print("{}\n", .{r.expression.body.group.items[1]});
        try testing.expectEqual(Expression.Body.Tag.select, r.expression.body);
        try testing.expectEqual(@as(usize, 2), r.expression.body.select.items.len);
        const sel1 = r.expression.body.select.items[0];
        try testing.expectEqual(Expression.Body.Tag.group, sel1.body);
        try testing.expectEqual(Expression.Body.Tag.string, sel1.body.group.items[0].body);
        try testing.expectEqual(Expression.Body.Tag.set, sel1.body.group.items[1].body);

        const sel2 = r.expression.body.select.items[1];
        try testing.expectEqual(Expression.Body.Tag.group, sel2.body);
        try testing.expectEqual(Expression.Body.Tag.string, sel2.body.group.items[0].body);
        try testing.expectEqual(Expression.Body.Tag.set, sel2.body.group.items[1].body);
    }
}

/// Grammar   <- Spacing Definition+ EndOfFile
pub fn parseGrammar(self: *PegParser) !Grammar {
    var result = Grammar{};
    _ = try self.runParser(spacing);
    while (true) {
        const rule = self.parseDefinition() catch |e| switch (e) {
            error.RuleEnd => continue,
            else => return e,
        };
        std.log.debug("\n\nparseGrammar() rule {s} <- {}\n", .{ rule.identifier, rule.expression });
        try result.rules.append(self.allocator, rule);
        if (self.stream.eof()) break;
    }
    return result;
}

test parseGrammar {
    const input =
        \\Sequence  <- Prefix (Prefix)* /
        \\Prefix    <- AND Suffix
        \\     / NOT Suffix
        \\     /     Suffix
    ;
    var output: [input.len]u8 = undefined;
    var p = init(testing.allocator, input, &output, "<test>");
    var g = try p.parseGrammar();
    defer g.deinit(testing.allocator);
    try testing.expectEqual(@as(usize, 2), g.rules.items.len);
    try testing.expect(g.rules.items[0].expression.body == .group);
    try testing.expectEqual(@as(usize, 2), g.rules.items[0].expression.body.group.items.len);
    try testing.expect(g.rules.items[1].expression.body == .select);
    try testing.expectEqual(@as(usize, 3), g.rules.items[1].expression.body.select.items.len);
}
