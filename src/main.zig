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

pub const PegResult = struct {
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

        pub const Modifiers = struct {
            optional: bool = false,
            zero_or_more: bool = false,
            one_or_more: bool = false,
            positive_lookahead: bool = false,
            negative_lookahead: bool = false,
        };

        body: Body,
        modifiers: Modifiers,
    };

    pub const Rule = struct {
        identifier: []const u8,
        expression: Expression,
    };

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
        for (self.stream.sliceToEnd()) |c, i| {
            if (!std.ascii.isAlphanumeric(c) and c != '_') {
                if (i == 0) return null;
                const id = try self.stream.read(i);
                std.log.info("Identifier: {s}", .{id});
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
            const expr = self.expression(false) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };

            try peg.rules.append(self.allocator, .{ .identifier = identifier, .expression = expr });
        }
        return peg;
    }

    const ModChar = enum { none, optional, zero_or_more, one_or_more };

    /// Consumes it if it exists
    fn modChar(self: *PegParser) error{EndOfStream}!ModChar {
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

    fn expression(self: *PegParser, single: bool) ParseError!PegResult.Expression {
        // a / b / c become three slash_bufs items
        var expressions = std.ArrayListUnmanaged(PegResult.Expression){};
        var expressions_kind: enum { group, select } = .group;

        var li: usize = 0;

        while (true) : (li += 1) {
            if (single and li > 0) break;
            const index_initial = self.stream.index;

            var expr = PegResult.Expression{ .body = undefined, .modifiers = .{} };

            self.skipWhitespaceAndComments() catch break;

            if (try self.readIdentifier()) |id| {
                self.skipWhitespace() catch break;
                if (std.mem.eql(u8, "<-", try self.stream.sliceBy(2))) {
                    self.stream.index = index_initial;
                    break;
                }
                expr.body = .{ .identifier = id };
            } else {
                var char = (try self.stream.read(1))[0];
                switch (char) {
                    '(' => {
                        expr = try self.expression(false);
                        self.skipWhitespaceAndComments() catch break;
                    },
                    ')' => {
                        break;
                    },
                    '[' => {
                        expr.body = .{ .set = try self.squareSet() };
                    },
                    '!' => {
                        expr = try self.expression(true);
                        expr.modifiers.negative_lookahead = true;
                    },
                    '.' => {
                        expr.body = .{ .any = {} };
                    },
                    '"', '\'' => {
                        var str = try self.string(if (char == '"') .double else .single);
                        expr.body = .{ .string = str.toOwnedSlice(self.allocator) };
                    },
                    else => return error.UnexpectedToken,
                }
            }

            while (true) {
                var mod = try self.modChar();
                switch (mod) {
                    .none => break,
                    .optional => expr.modifiers.optional = true,
                    .zero_or_more => expr.modifiers.zero_or_more = true,
                    .one_or_more => expr.modifiers.one_or_more = true,
                }
            }

            self.skipWhitespaceAndComments() catch break;

            var slash_char = try self.stream.peek(0);
            if (slash_char == '/') {
                try self.stream.consume(1);
                expressions_kind = .select;
            }
        }

        if (expressions.items.len == 1) {
            defer expressions.deinit(self.allocator);
            return expressions.items[0];
        } else return .{ .body = switch (expressions_kind) {
            .group => .{ .group = expressions },
            .select => .{ .select = expressions },
        }, .modifiers = .{} };
    }

    fn escapeToChar(self: *PegParser) error{EndOfStream}!u8 {
        var first = (try self.stream.read(1))[0];

        return if (std.ascii.isDigit(first)) d: {
            self.stream.index -= 1;
            break :d std.fmt.parseInt(u8, try self.stream.read(3), 8) catch @panic("bruh");
        } else switch (first) {
            'n' => '\n',
            't' => '\t',
            '"' => '"',
            '\'' => '\'',
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

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var out = try std.fs.cwd().createFile("out.zig", .{});
    defer out.close();

    var grammar = try std.fs.cwd().openFile("grammar.y", .{});
    defer grammar.close();

    var data = try grammar.readToEndAlloc(allocator, 50_000);
    defer allocator.free(data);

    var gen = PegParser.init(allocator, data);
    const p = try gen.parse();
    std.log.info("Printing", .{});
    std.log.info("{any}", .{p});
}
