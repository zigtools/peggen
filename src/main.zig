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

pub const Generator = struct {
    allocator: std.mem.Allocator,
    stream: Stream,
    iterator_index: usize = 0,

    pub fn init(allocator: std.mem.Allocator, data: []const u8) Generator {
        return .{
            .allocator = allocator,
            .stream = Stream{ .buffer = data },
        };
    }

    fn skipWhitespace(self: *Generator) error{EndOfStream}!void {
        while (std.ascii.isWhitespace(try self.stream.peek(0))) try self.stream.consume(1);
    }

    fn skipWhitespaceAndComments(self: *Generator) error{EndOfStream}!void {
        try self.skipWhitespace();
        while ((try self.stream.peek(0)) == '#') {
            try self.skipWhitespace();
            while ((try self.stream.peek(0)) != '\n') try self.stream.consume(1);
            try self.skipWhitespace();
        }
        try self.skipWhitespace();
    }

    fn readIdentifier(self: *Generator) error{EndOfStream}!?[]const u8 {
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

    fn discardArrow(self: *Generator) error{ UnexpectedToken, EndOfStream }!void {
        if (!std.mem.eql(u8, "<-", try self.stream.sliceBy(2)))
            return error.UnexpectedToken;

        try self.stream.consume(2);
    }

    pub fn generate(self: *Generator, writer: anytype) anyerror!void {
        try writer.writeAll(@embedFile("base.zig"));

        while (true) {
            self.skipWhitespaceAndComments() catch break;
            var identifier = (self.readIdentifier() catch break) orelse return error.UnexpectedToken;
            self.skipWhitespaceAndComments() catch break;
            try self.discardArrow();
            try writer.print("pub fn {s}(writer: anytype, ctx: *GenerationContext) anyerror!void {{_ = .{{writer, random}};", .{std.zig.fmtId(identifier)});
            self.expression(writer) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            try writer.writeAll("}");
        }
    }

    fn expression(self: *Generator, writer: anytype) anyerror!void {
        // a / b / c become three slash_bufs items
        var slash_bufs = std.ArrayListUnmanaged(std.ArrayList(u8)){};
        defer slash_bufs.deinit(self.allocator);

        try slash_bufs.append(self.allocator, std.ArrayList(u8).init(self.allocator));

        var slash_buf = &slash_bufs.items[slash_bufs.items.len - 1];

        while (true) {
            const index_initial = self.stream.index;
            // Used to store the "body" before modifications a? would lead to a being stored
            var buf = std.ArrayList(u8).init(self.allocator);

            self.skipWhitespace() catch break;

            if (try self.readIdentifier()) |id| {
                self.skipWhitespace() catch break;
                if (std.mem.eql(u8, "<-", try self.stream.sliceBy(2))) {
                    self.stream.index = index_initial;
                    break;
                }
                try buf.writer().print("try {s}(writer, ctx);", .{std.zig.fmtId(id)});
            } else {
                var char = (try self.stream.read(1))[0];
                switch (char) {
                    '(' => {
                        try buf.writer().writeByte('{');
                        try self.expression(buf.writer());
                        self.skipWhitespace() catch break;
                        self.skipWhitespaceAndComments() catch break;
                        try buf.writer().writeByte('}');
                    },
                    ')' => {
                        break;
                    },
                    '[' => {
                        try self.squareSet(buf.writer());
                    },
                    '!' => {
                        try self.expression(std.io.null_writer);
                    },
                    '.' => {
                        try buf.writer().writeAll("try writer.writeByte(random.intRangeLessThan(u8, 32, 126));");
                    },
                    '"', '\'' => {
                        try self.string(buf.writer(), if (char == '"') .double else .single);
                    },
                    '#' => {
                        self.stream.index -= 1;
                        break;
                    },
                    else => return error.UnexpectedToken,
                }
            }

            // Only assuming one at most is present
            var mod_char = try self.stream.peek(0);
            switch (mod_char) {
                '?' => {
                    try self.stream.consume(1);

                    try slash_buf.writer().writeAll("if (ctx.randomBool()) {");
                    try slash_buf.appendSlice(buf.items);
                    try slash_buf.writer().writeAll("}");
                },
                '*' => {
                    try self.stream.consume(1);

                    try slash_buf.writer().print("var id{d}: usize = 0; const b{d} = ctx.random.intRangeAtMost(usize, 0, 128); while (id{d} < b{d}) : (id{d} += 1) {{", .{ self.iterator_index, self.iterator_index, self.iterator_index, self.iterator_index, self.iterator_index });
                    self.iterator_index += 1;
                    try slash_buf.appendSlice(buf.items);
                    try slash_buf.writer().writeAll("}");
                },
                '+' => {
                    try self.stream.consume(1);

                    try slash_buf.writer().print("var id{d}: usize = 0; const b{d} = ctx.random.intRangeAtMost(usize, 0, 128); while (id{d} < b{d}) : (id{d} += 1) {{", .{ self.iterator_index, self.iterator_index, self.iterator_index, self.iterator_index, self.iterator_index });
                    self.iterator_index += 1;
                    try slash_buf.appendSlice(buf.items);
                    try slash_buf.writer().writeAll("}");
                },
                else => {
                    try slash_buf.appendSlice(buf.items);
                },
            }

            self.skipWhitespaceAndComments() catch break;

            var slash_char = try self.stream.peek(0);
            if (slash_char == '/') {
                try self.stream.consume(1);
                try slash_bufs.append(self.allocator, std.ArrayList(u8).init(self.allocator));
                slash_buf = &slash_bufs.items[slash_bufs.items.len - 1];
            }
        }

        if (slash_bufs.items.len == 1) {
            try writer.writeAll(slash_bufs.items[0].items);
        } else {
            try writer.print("switch (random.intRangeLessThan(usize, 0, {d})) {{", .{slash_bufs.items.len});
            for (slash_bufs.items) |*z, i| {
                try writer.print("{d} => {{", .{i});
                try writer.writeAll(z.items);
                try writer.writeAll("},");
            }
            try writer.writeAll("else => unreachable,");
            try writer.writeAll("}");
        }
    }

    fn escapeToChar(self: *Generator) error{EndOfStream}!u8 {
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

    fn charOrEscapeToChar(self: *Generator, char: u8) error{EndOfStream}!u8 {
        return if (char == '\\') self.escapeToChar() else char;
    }

    const QuoteKind = enum(u8) { single = '\'', double = '"' };
    fn string(self: *Generator, writer: anytype, quote_kind: QuoteKind) error{ OutOfMemory, EndOfStream }!void {
        var string_buf = std.ArrayListUnmanaged(u8){};
        defer string_buf.deinit(self.allocator);

        while (true) {
            const char = (try self.stream.read(1))[0];

            if (char == @enumToInt(quote_kind)) {
                try writer.print("try writer.writeAll(\"{}\");", .{std.zig.fmtEscapes(string_buf.items)});
                break;
            }

            try string_buf.append(self.allocator, try self.charOrEscapeToChar(char));
        }
    }

    fn squareSet(self: *Generator, writer: anytype) error{ OutOfMemory, EndOfStream }!void {
        var validity_list = std.ArrayListUnmanaged(u8){};
        defer validity_list.deinit(self.allocator);

        var negate = false;

        while (true) {
            const char = (try self.stream.read(1))[0];

            if (char == '^') negate = true;

            switch (char) {
                '-' => {
                    if (validity_list.items.len == 0) {
                        try validity_list.append(self.allocator, '-');
                        continue;
                    }
                    const start = validity_list.pop();
                    const end_tok = (try self.stream.read(1))[0];
                    const end = try self.charOrEscapeToChar(end_tok);

                    var i: u8 = start;
                    while (i <= end) : (i += 1) {
                        try validity_list.append(self.allocator, i);
                    }
                },
                ']' => {
                    if (!negate) {
                        try writer.writeAll("try writer.writeByte(\"");
                        try writer.print("{}", .{std.zig.fmtEscapes(validity_list.items)});
                        try writer.print("\"[random.intRangeLessThan(u8, 0, {d})]);", .{validity_list.items.len});
                    }
                    break;
                },
                else => {
                    try validity_list.append(self.allocator, try self.charOrEscapeToChar(char));
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

    var writer = out.writer();
    var gen = Generator.init(allocator, data);
    gen.generate(writer) catch |err| switch (err) {
        error.UnexpectedToken => {
            std.log.err("{d}: {c}", .{ gen.stream.index, gen.stream.buffer[gen.stream.index] });
            return err;
        },
        else => return err,
    };
}
