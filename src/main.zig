const std = @import("std");

pub const Stream = struct {
    index: usize = 0,
    buffer: []const u8,

    pub fn peek(self: *Stream, by: usize) error{EndOfStream}!u8 {
        if (self.index + by >= self.buffer.len) return error.EndOfStream;
        return self.buffer[self.index + by];
    }

    pub fn consume(self: *Stream, count: usize) error{EndOfStream}!void {
        if (self.index + count >= self.buffer.len) return error.EndOfStream;
        self.index += count;
    }

    pub fn sliceToEnd(self: *Stream) []const u8 {
        return self.buffer[self.index..];
    }

    pub fn sliceBy(self: *Stream, count: usize) error{EndOfStream}![]const u8 {
        if (self.index + count >= self.buffer.len) return error.EndOfStream;
        return self.buffer[self.index .. self.index + count];
    }

    pub fn read(self: *Stream, count: usize) error{EndOfStream}![]const u8 {
        const res = try self.sliceBy(count);
        try self.consume(count);
        return res;
    }
};

pub fn isIdentifierCharacter(char: u8) bool {
    return std.ascii.isAlphanumeric(char) or char == '_';
}

pub const Expression = struct {
    pub const Body = union(enum) {
        /// .
        any,
        identifier: []const u8,
        /// "characters" 'characters'
        string: []const u8,
        /// [a-zA-Z_] [^0-9]
        set: std.ArrayListUnmanaged(u8),
        /// (abc def)
        group: std.ArrayListUnmanaged(Expression),
        /// abc / def / gej
        select: std.ArrayListUnmanaged(Expression),
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

    fn simplify(expr: *Expression, allocator: std.mem.Allocator) void {
        switch (expr.body) {
            .group, .select => |*entries| {
                if (entries.items.len == 1 and expr.lookahead == .none and expr.modifier == .none) {
                    const single = entries.items[0];
                    entries.deinit(allocator);
                    expr.* = single;
                    expr.simplify(allocator);
                }
            },
            else => {},
        }
    }

    pub fn format(expr: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        switch (expr.modifier) {
            .none => {},
            .optional => try writer.writeAll("modifiers.optional("),
            .zero_or_more => try writer.writeAll("modifiers.zeroOrMore("),
            .one_or_more => try writer.writeAll("modifiers.oneOrMore("),
        }

        switch (expr.body) {
            .any => try writer.writeAll("rules.any()"),
            .identifier => |id| try writer.print("{}", .{std.zig.fmtId(id)}),
            .string => |str| try writer.print("\"{}\"", .{std.zig.fmtEscapes(str)}),
            .set => |set| {
                _ = set;
                try writer.writeAll("rules.set(.{})");
            },
            .group => |group| {
                try writer.writeAll("rules.group(.{");
                for (group.items) |sub_expr| {
                    try writer.print("{},", .{sub_expr});
                }
                try writer.writeAll("})");
            },
            .select => |select| {
                try writer.writeAll("rules.select(.{");
                for (select.items) |sub_expr| {
                    try writer.print("{},", .{sub_expr});
                }
                try writer.writeAll("})");
            },
        }

        switch (expr.modifier) {
            .none => {},
            else => try writer.writeAll(")"),
        }
    }
};

pub const Rule = struct {
    identifier: []const u8,
    expression: Expression,
};

pub const PegResult = struct {
    rules: std.ArrayListUnmanaged(Rule),
};

pub const PegParser = struct {
    allocator: std.mem.Allocator,
    stream: Stream,

    pub const ParseError = error{ OutOfMemory, EndOfStream, UnexpectedToken };

    pub fn init(allocator: std.mem.Allocator, data: []const u8) PegParser {
        return .{
            .allocator = allocator,
            .stream = Stream{ .buffer = data },
        };
    }

    fn skipWhitespace(self: *PegParser) error{EndOfStream}!void {
        while (std.ascii.isWhitespace(try self.stream.peek(0))) try self.stream.consume(1);
    }

    fn skipWhitespaceAndComments(self: *PegParser) error{EndOfStream}!void {
        try self.skipWhitespace();
        while ((try self.stream.peek(0)) == '#') {
            try self.skipWhitespace();
            while ((try self.stream.peek(0)) != '\n') try self.stream.consume(1);
            try self.skipWhitespace();
        }
        try self.skipWhitespace();
    }

    fn readIdentifier(self: *PegParser) error{EndOfStream}!?[]const u8 {
        for (self.stream.sliceToEnd(), 0..) |c, i| {
            if (!std.ascii.isAlphanumeric(c) and c != '_') {
                if (i == 0) return null;
                const id = try self.stream.read(i);
                return id;
            }
        }

        return error.EndOfStream;
    }

    fn discardArrow(self: *PegParser) error{ EndOfStream, UnexpectedToken }!void {
        if (!std.mem.eql(u8, "<-", try self.stream.sliceBy(2)))
            return error.UnexpectedToken;

        try self.stream.consume(2);
    }

    pub fn parse(self: *PegParser) ParseError!PegResult {
        var peg = PegResult{
            .rules = .{},
        };
        while (true) {
            self.skipWhitespaceAndComments() catch break;
            const identifier = (self.readIdentifier() catch break) orelse return error.UnexpectedToken;
            self.skipWhitespaceAndComments() catch break;
            try self.discardArrow();
            const expr = self.expression() catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };

            try peg.rules.append(self.allocator, .{ .identifier = identifier, .expression = expr });
        }
        return peg;
    }

    /// Consumes it if it exists
    fn modChar(self: *PegParser) error{EndOfStream}!Expression.Modifier {
        // Only assuming one at most is present
        var mod_char = try self.stream.peek(0);
        switch (mod_char) {
            '?' => {
                try self.stream.consume(1);
                return .optional;
            },
            '*' => {
                try self.stream.consume(1);
                return .zero_or_more;
            },
            '+' => {
                try self.stream.consume(1);
                return .one_or_more;
            },
            else => return .none,
        }
    }

    fn singleExpression(self: *PegParser) ParseError!?Expression {
        const index_initial = self.stream.index;

        var expr = Expression{ .body = undefined };

        try self.skipWhitespaceAndComments();

        if (try self.readIdentifier()) |id| {
            try self.skipWhitespace();
            if (std.mem.eql(u8, "<-", try self.stream.sliceBy(2))) {
                self.stream.index = index_initial;
                return null;
            }
            expr.body = .{ .identifier = id };
        } else {
            var char = (try self.stream.read(1))[0];
            switch (char) {
                '(' => {
                    expr = try self.expression();
                    try self.skipWhitespaceAndComments();
                },
                ')' => {
                    self.stream.index -= 1;
                    return null;
                },
                '[' => {
                    expr.body = .{ .set = try self.squareSet() };
                },
                '&' => {
                    expr = try self.singleExpression() orelse return null;
                    if (expr.lookahead != .none)
                        return error.UnexpectedToken;
                    expr.lookahead = .positive;
                },
                '!' => {
                    expr = try self.singleExpression() orelse return null;
                    if (expr.lookahead != .none)
                        return error.UnexpectedToken;
                    expr.lookahead = .negative;
                },
                '.' => {
                    expr.body = .{ .any = {} };
                },
                '"', '\'' => {
                    var str = try self.string(if (char == '"') .double else .single);
                    expr.body = .{ .string = try str.toOwnedSlice(self.allocator) };
                },
                else => {
                    std.log.err("Unexpected character '{c}' @ {d}", .{ char, self.stream.index });
                    return error.UnexpectedToken;
                },
            }

            if (char == '(') {
                try self.skipWhitespaceAndComments();
                if ((try self.stream.read(1))[0] != ')')
                    return error.UnexpectedToken;
            }
        }

        var mod = try self.modChar();
        switch (mod) {
            .none => {},
            else => {
                expr.modifier = mod;
                if (try self.modChar() != .none)
                    return error.UnexpectedToken;
            },
        }

        try self.skipWhitespaceAndComments();

        return expr;
    }

    fn expression(self: *PegParser) ParseError!Expression {
        var group = std.ArrayListUnmanaged(Expression){};
        var selections = std.ArrayListUnmanaged(Expression){};

        while (true) {
            var expr = try self.singleExpression() orelse {
                try selections.append(self.allocator, .{
                    .body = .{
                        .group = try group.clone(self.allocator),
                    },
                });
                break;
            };
            try group.append(self.allocator, expr);

            var slash_char = try self.stream.peek(0);
            if (slash_char == '/') {
                try self.stream.consume(1);

                try selections.append(self.allocator, .{
                    .body = .{
                        .group = try group.clone(self.allocator),
                    },
                });
                group.items.len = 0;
            }
        }

        var expr = Expression{
            .body = .{ .select = selections },
        };
        expr.simplify(self.allocator);
        return expr;
    }

    fn escapeToChar(self: *PegParser) error{EndOfStream}!u8 {
        var first = (try self.stream.read(1))[0];

        return if (std.ascii.isDigit(first)) d: {
            var start = self.stream.index - 1;
            while (std.ascii.isDigit((try self.stream.read(1))[0])) {}
            self.stream.index -= 1;
            break :d std.fmt.parseInt(u8, self.stream.buffer[start..self.stream.index], 8) catch @panic("bruh");
        } else switch (first) {
            'a' => 0x07,
            'b' => 0x08,
            'e' => 0x1B,
            'f' => 0x0C,
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => 0x0B,
            '\'' => '\'',
            '"' => '"',
            '[' => '[',
            ']' => ']',
            '\\' => '\\',
            else => unreachable,
        };
    }

    fn charOrEscapeToChar(self: *PegParser, char: u8) error{EndOfStream}!u8 {
        return if (char == '\\') self.escapeToChar() else char;
    }

    const QuoteKind = enum(u8) { single = '\'', double = '"' };
    fn string(self: *PegParser, quote_kind: QuoteKind) ParseError!std.ArrayListUnmanaged(u8) {
        var string_buf = std.ArrayListUnmanaged(u8){};

        while (true) {
            const char = (try self.stream.read(1))[0];

            if (char == @enumToInt(quote_kind)) {
                return string_buf;
            }

            try string_buf.append(self.allocator, try self.charOrEscapeToChar(char));
        }
    }

    fn squareSet(self: *PegParser) ParseError!std.ArrayListUnmanaged(u8) {
        var validity_map = std.AutoArrayHashMapUnmanaged(u8, void){};
        defer validity_map.deinit(self.allocator);

        var negate = false;
        var last: u8 = 0;

        while (true) {
            const char = (try self.stream.read(1))[0];
            defer last = char;

            if (char == '^') negate = true;

            switch (char) {
                '-' => {
                    if (validity_map.count() == 0) {
                        try validity_map.put(self.allocator, '-', {});
                        continue;
                    }
                    const start = last;
                    const end_tok = (try self.stream.read(1))[0];
                    const end = try self.charOrEscapeToChar(end_tok);

                    var i: u8 = start;
                    while (i <= end) : (i += 1) {
                        try validity_map.put(self.allocator, i, {});
                    }
                },
                ']' => {
                    var list = std.ArrayListUnmanaged(u8){};
                    try list.ensureTotalCapacity(self.allocator, if (!negate) (validity_map.count()) else (256 - validity_map.count()));

                    if (!negate) {
                        var it = validity_map.iterator();
                        while (it.next()) |v| try list.append(self.allocator, v.key_ptr.*);
                    } else {
                        var i: u8 = 0;
                        while (true) : (i += 1) {
                            if (!validity_map.contains(i))
                                try list.append(self.allocator, i);
                            if (i == 255) break;
                        }
                    }

                    return list;
                },
                else => {
                    try validity_map.put(self.allocator, try self.charOrEscapeToChar(char), {});
                },
            }
        }
    }
};

pub fn generate(result: PegResult, writer: anytype) !void {
    for (result.rules.items) |rule| {
        try writer.print("const {} = {};\n", .{ std.zig.fmtId(rule.identifier), rule.expression });
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var out = try std.fs.cwd().createFile("out.zig", .{});
    defer out.close();

    var grammar = try std.fs.cwd().openFile("examples/peg.peg", .{});
    defer grammar.close();

    var data = try grammar.readToEndAlloc(allocator, 50_000);
    defer allocator.free(data);

    var gen = PegParser.init(allocator, data);
    const p = try gen.parse();

    try generate(p, out.writer());
}
