const std = @import("std");
const mem = std.mem;
const Stream = @import("Stream.zig");

pub const Context = struct {
    file_path: []const u8,
    flags: Flags = Flags.initEmpty(),

    const Flags = std.enums.EnumSet(Flag);
    const Flag = enum { print_errors };

    pub fn addError(ctx: *Context, stream: *Stream, rule: []const u8, err: anyerror) void {
        if (!ctx.flags.contains(.print_errors)) return;
        var line: usize = 1;
        var col: usize = 1;
        var line_start: usize = 0;
        for (stream.input[0..stream.index], 0..) |c, i| {
            const is_nl = c == '\n';
            line += @boolToInt(is_nl);
            if (is_nl) {
                line_start = i + 1;
                col = 1;
            } else col += 1;
        }
        var line_end = stream.index;
        while (line_end < stream.input.len) : (line_end += 1) {
            const c = stream.input[line_end];
            if (c == '\n') break;
        }
        std.debug.print("{s}:{}:{}: rule '{s}' error.{s}\n", .{ ctx.file_path, line, col, rule, @errorName(err) });
        std.debug.print("{s}\n", .{stream.input[line_start..line_end]});
        for (0..col) |_| std.debug.print(" ", .{});
        std.debug.print("^\n", .{});
    }
};

pub const Error = error{ ParseFailure, NoSpaceLeft };

pub fn Char(comptime rule: @TypeOf(.enum_literal), comptime c: u8) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            _ = ctx;
            const actual = stream.readByte() catch
                return error.ParseFailure;
            if (actual != c) {
                stream.index -= 1;
                return error.ParseFailure;
            }
            try stream.writeByte(c);
        }
    };
}

pub fn CharFn(comptime rule: @TypeOf(.enum_literal), comptime charFn: fn (u8) bool) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            _ = ctx;
            const c = stream.readByte() catch
                return error.ParseFailure;
            if (!charFn(c)) {
                stream.index -= 1;
                return error.ParseFailure;
            }
            try stream.writeByte(c);
        }
    };
}

pub fn CharRange(comptime rule: @TypeOf(.enum_literal), comptime start: u8, comptime end: u8) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            _ = ctx;
            const c = stream.readByte() catch
                return error.ParseFailure;
            // TODO make this branchless
            // start > c
            // c > end
            if (start > c or c > end) {
                stream.index -= 1;
                return error.ParseFailure;
            }
            try stream.writeByte(c);
        }
    };
}

inline fn matchString(comptime expected: []const u8, actual: []const u8) bool {
    // TODO optimize
    return mem.eql(u8, expected, actual);
}

pub fn String(comptime rule: @TypeOf(.enum_literal), comptime expected: []const u8) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            _ = ctx;
            const index = stream.index;
            const actual = stream.read(expected.len) catch
                return error.ParseFailure;
            if (!matchString(expected, actual)) {
                stream.index = index;
                return error.ParseFailure;
            }
            _ = try stream.write(expected);
        }
    };
}

pub fn Select(comptime rule: @TypeOf(.enum_literal), comptime parsers: anytype) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            const state = stream.checkpoint();

            inline for (parsers) |parser| {
                if (parser.match(stream, ctx)) |_| return else |_| {}
                stream.restore(state);
            }
            return error.ParseFailure;
        }
    };
}

pub fn Group(comptime rule: @TypeOf(.enum_literal), comptime parsers: anytype) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            // std.log.debug("Group {s}", .{rule_name});
            const state = stream.checkpoint();
            errdefer stream.restore(state);
            inline for (parsers) |parser|
                try parser.match(stream, ctx);
        }
    };
}

pub fn MoreThanN(
    comptime rule: @TypeOf(.enum_literal),
    comptime n: comptime_int,
    comptime parser: anytype,
) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            inline for (0..n) |_|
                try parser.match(stream, ctx);

            while (true) {
                const state = stream.checkpoint();
                parser.match(stream, ctx) catch {
                    stream.restore(state);
                    break;
                };
            }
        }
    };
}

pub fn ZeroOrMore(comptime rule: @TypeOf(.enum_literal), comptime parser: anytype) type {
    return MoreThanN(rule, 0, parser);
}

pub fn OneOrMore(comptime rule: @TypeOf(.enum_literal), comptime parser: anytype) type {
    return MoreThanN(rule, 1, parser);
}

pub fn Optional(
    comptime rule: @TypeOf(.enum_literal),
    comptime parser: anytype,
) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            parser.match(stream, ctx) catch {};
        }
    };
}

pub fn Negative(
    comptime rule: @TypeOf(.enum_literal),
    comptime parser: anytype,
) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            const state = stream.checkpoint();
            defer stream.restore(state);

            if (parser.match(stream, ctx)) |_|
                return error.ParseFailure
            else |_| {}
        }
    };
}

pub fn Positive(
    comptime rule: @TypeOf(.enum_literal),
    comptime parser: anytype,
) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            const state = stream.checkpoint();
            defer stream.restore(state);
            parser.match(stream, ctx) catch
                return error.ParseFailure;
        }
    };
}

pub fn Any(
    comptime rule: @TypeOf(.enum_literal),
) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            _ = ctx;
            _ = stream.readByte() catch return error.ParseFailure;
        }
    };
}

pub fn AnyOf(
    comptime rule: @TypeOf(.enum_literal),
    comptime elements: []const u8,
) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) Error!void {
            _ = ctx;
            const c = stream.readByte() catch
                return error.ParseFailure;
            for (elements) |c2|
                if (c == c2) return;
            return error.ParseFailure;
        }
    };
}

pub fn EscapeFn(comptime Err: type) type {
    return fn ([]const u8, *usize) Err!u8;
}

pub fn Escape(
    comptime rule: @TypeOf(.enum_literal),
    comptime parser: anytype,
    comptime EscapeErr: type,
    comptime escapeFn: EscapeFn(EscapeErr),
) type {
    return struct {
        pub const rule_name = @tagName(rule);
        pub fn match(stream: *Stream, ctx: *Context) (EscapeErr || Error)!void {
            const index = stream.index;
            const flags = stream.flags;
            errdefer stream.flags = flags;
            stream.flags.insert(.suppress_output);
            try parser.match(stream, ctx);
            var len: usize = 0;
            const escaped = try escapeFn(stream.input[index..], &len);
            stream.flags = flags;
            try stream.writeByte(escaped);
        }
    };
}

pub fn parse(comptime Parser: type, stream: *Stream, ctx: *Context) ![]const u8 {
    const out_pos = stream.output.pos;
    Parser.match(stream, ctx) catch |e| {
        ctx.addError(stream, Parser.rule_name, e);
        return e;
    };
    return stream.output.buffer[out_pos..stream.output.pos];
}

pub fn Iterator(comptime P: type) type {
    return struct {
        stream: *Stream,
        ctx: *Context,
        pub const Parser = P;
        const Self = @This();
        pub fn next(self: *Self) !?[]const u8 {
            if (self.stream.eof()) return null;
            return try parse(Parser, self.stream, self.ctx);
        }

        pub fn reset(self: *Self) void {
            self.stream.index = 0;
            self.stream.output.pos = 0;
        }
        pub fn count(self: *Self) !usize {
            var result: usize = 0;
            while (try self.next()) |_| : (result += 1) {}
            return result;
        }
    };
}

pub fn iterator(comptime Parser: type, stream: *Stream, ctx: *Context) Iterator(Parser) {
    return Iterator(Parser){ .stream = stream, .ctx = ctx };
}

const testing = std.testing;

/// returns parse result and remainder. parse result is an error union to allow
/// checking error states
fn doParse(
    input: []const u8,
    output: []u8,
    parser: anytype,
) struct { anyerror![]const u8, []const u8 } {
    var s = Stream.init(input, output);
    var ctx = Context{ .file_path = "<testing>" };
    const err_or_void = parse(parser, &s, &ctx);
    return .{ err_or_void, s.input[s.index..] };
}

fn expectParse(input: []const u8, parser: anytype, expected_result: []const u8) !void {
    var output: [0x100]u8 = undefined;
    const result = doParse(input, &output, parser);
    _ = try result[0];
    try testing.expectEqualStrings(expected_result, result[1]);
}

fn expectFailure(input: []const u8, parser: anytype, err: anyerror) !void {
    var output: [0x100]u8 = undefined;
    const result = doParse(input, &output, parser);
    try testing.expectError(err, result[0]);
    try testing.expectEqualStrings(input, result[1]);
}

test Char {
    const f = Char(.f, 'f');
    try expectParse("foo", f, "oo");
    try expectParse("f", f, "");
    try expectFailure("", f, error.ParseFailure);
}

const ws = CharFn(.ws, std.ascii.isWhitespace);
test CharFn {
    try expectParse(" ", ws, "");
    try expectParse("\n--", ws, "--");
    try expectFailure("a", ws, error.ParseFailure);
}

test CharRange {
    const az = CharRange(.az, 'a', 'z');
    try expectParse("a", az, "");
    try expectParse("z--", az, "--");
    try expectFailure(&[_]u8{'a' - 1}, az, error.ParseFailure);
    try expectFailure(&[_]u8{'z' + 1}, az, error.ParseFailure);
}

const foo = String(.foo, "foo");
const bar = String(.bar, "bar");
test String {
    try expectParse("foo", foo, "");
    try expectParse("fooba", foo, "ba");
    try expectParse("bar", bar, "");
    try expectParse("bar--", bar, "--");
    try expectFailure("fo", foo, error.ParseFailure);
    try expectFailure("ba", foo, error.ParseFailure);
    try expectFailure("", foo, error.ParseFailure);
}

test Select {
    const foo_bar = Select(.foo_bar, .{ foo, bar });
    try expectParse("foo", foo_bar, "");
    try expectParse("fooba", foo_bar, "ba");
    try expectParse("bar", foo_bar, "");
    try expectParse("bar--", foo_bar, "--");
    try expectFailure("fo", foo_bar, error.ParseFailure);
    try expectFailure("ba", foo_bar, error.ParseFailure);
    try expectFailure("", foo_bar, error.ParseFailure);
}

test Group {
    const foo_bar = Group(.foo_bar, .{ foo, bar });
    try expectParse("foobar", foo_bar, "");
    try expectParse("foobar--", foo_bar, "--");
    try expectFailure("", foo_bar, error.ParseFailure);
    try expectFailure("fo", foo_bar, error.ParseFailure);
    try expectFailure("foo", foo_bar, error.ParseFailure);
    try expectFailure("foob", foo_bar, error.ParseFailure);
    try expectFailure("fooba", foo_bar, error.ParseFailure);
}

test ZeroOrMore {
    const many_ws = ZeroOrMore(.ws, ws);
    try expectParse("  \n\r\t--", many_ws, "--");
    try expectParse("--", many_ws, "--");
}

test OneOrMore {
    const some_ws = OneOrMore(.ws, ws);
    try expectParse("  \n\r\t--", some_ws, "--");
    try expectFailure("--", some_ws, error.ParseFailure);
    try expectFailure("", some_ws, error.ParseFailure);
}

test Negative {
    const ident = OneOrMore(.ident, CharFn(.alphanum, std.ascii.isAlphanumeric));
    const leftarrow = String(.leftarrow, "<--");
    const p1 = Group(.primary1, .{ ident, Negative(.not_leftarrow, leftarrow) });
    try expectParse("foo--", p1, "--");
    try expectFailure("foo<----", p1, error.ParseFailure);
}

test Positive {
    const ident = OneOrMore(.ident, CharFn(.alphanum, std.ascii.isAlphanumeric));
    const leftarrow = String(.leftarrow, "<--");
    const p1 = Group(.primary1, .{ ident, Positive(.not_leftarrow, leftarrow) });
    try expectFailure("foo--", p1, error.ParseFailure);
    try expectParse("foo<----", p1, "<----");
}
