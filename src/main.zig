const std = @import("std");
const ptk = @import("parser-toolkit");
const ptkm = ptk.matchers;

fn validSquareBracketMatcher(str: []const u8) ?usize {
    return if (str[0] >= 32 and str[0] <= 126) 1 else null;
}

fn isEscapeChar(str: []const u8) ?usize {
    return switch (str[0]) {
        'n' => 1,
        't' => 1,
        '"' => 1,
        '\'' => 1,
        '\\' => 1,
        else => null,
    };
}

pub fn PegGen(comptime WriterType: type) type {
    return struct {
        const Self = @This();

        const TokenType = enum {
            comment,
            whitespace,
            identifier,

            @"<-",
            @"[",
            @"]",
            @"(",
            @")",
            @"?",
            @"!",
            @"/",
            @"-",
            @"+",
            @"*",
            @"\"",
            @"'",
            @".",

            square_bracket_element,
            alpha_escape_sequence,
            numeric_escape_sequence,
        };

        const Pattern = ptk.Pattern(TokenType);
        const ruleset = ptk.RuleSet(TokenType);

        const Tokenizer = ptk.Tokenizer(TokenType, &[_]Pattern{
            Pattern.create(.whitespace, ptkm.whitespace),
            Pattern.create(.comment, ptkm.sequenceOf(.{ ptkm.literal("#"), ptkm.takeNoneOf("\n"), ptkm.literal("\n") })),
            Pattern.create(.identifier, ptkm.identifier),

            Pattern.create(.alpha_escape_sequence, ptkm.sequenceOf(.{ ptkm.literal("\\"), isEscapeChar })),
            Pattern.create(.numeric_escape_sequence, ptkm.sequenceOf(.{ ptkm.literal("\\"), ptkm.octalNumber, ptkm.octalNumber, ptkm.octalNumber })),

            Pattern.create(.@"<-", ptkm.literal("<-")),
            Pattern.create(.@"[", ptkm.literal("[")),
            Pattern.create(.@"]", ptkm.literal("]")),
            Pattern.create(.@"(", ptkm.literal("(")),
            Pattern.create(.@")", ptkm.literal(")")),
            Pattern.create(.@"?", ptkm.literal("?")),
            Pattern.create(.@"!", ptkm.literal("!")),
            Pattern.create(.@"/", ptkm.literal("/")),
            Pattern.create(.@"-", ptkm.literal("-")),
            Pattern.create(.@"+", ptkm.literal("+")),
            Pattern.create(.@"*", ptkm.literal("*")),
            Pattern.create(.@"\"", ptkm.literal("\"")),
            Pattern.create(.@"'", ptkm.literal("'")),
            Pattern.create(.@".", ptkm.literal(".")),

            Pattern.create(.square_bracket_element, validSquareBracketMatcher),
        });

        const ParserCore = ptk.ParserCore(Tokenizer, .{
            .whitespace,
            .comment,
        });

        allocator: std.mem.Allocator,
        source: []const u8,
        core: ParserCore,
        ii: usize = 0,

        pub const GenerateError = WriterType.Error || std.mem.Allocator.Error || ParserCore.Error;

        pub fn generate(allocator: std.mem.Allocator, writer: WriterType, source: []const u8) GenerateError!void {
            var tokenizer = Tokenizer.init(source, null);
            var gen = Self{
                .source = source,
                .allocator = allocator,
                .core = ParserCore.init(&tokenizer),
            };
            try gen.acceptTopLevelExpression(writer);
        }

        pub fn acceptTopLevelExpression(gen: *Self, writer: WriterType) GenerateError!void {
            const state = gen.core.saveState();
            errdefer gen.core.restoreState(state);

            while (true) {
                const variable = try gen.core.accept(comptime ruleset.is(.identifier));
                _ = try gen.core.accept(comptime ruleset.is(.@"<-"));

                try writer.print("pub fn {s}(writer: anytype, random: Random) anyerror!void {{_ = .{{writer, random}};", .{std.zig.fmtId(variable.text)});
                try gen.acceptExpression(writer);
                try writer.writeAll("}");
            }
        }

        // pub fn translateEscape(gen: *Self, writer: anytype, escape: Tokenizer.Token) GenerateError!void {
        //     _ = gen;
        //     switch (escape.type) {
        //         .numeric_escape_sequence => try writer.writeAll("\\x{x}", .{try std.fmt.parseInt(usize, escape.text[1..], 8)}),
        //         else => try writer.writeAll(escape.text),
        //     }
        // }

        fn vliToU8(token: Tokenizer.Token) u8 {
            return switch (token.type) {
                .numeric_escape_sequence => std.fmt.parseInt(u8, token.text[1..], 8) catch @panic("bruh"),
                .alpha_escape_sequence => switch (token.text[1]) {
                    'n' => '\n',
                    't' => '\t',
                    '"' => '"',
                    '\'' => '\'',
                    '\\' => '\\',
                    else => unreachable,
                },
                .square_bracket_element, .identifier, .@"\"", .@"'" => token.text[0],
                else => if (token.text.len == 1) token.text[0] else {
                    std.log.err("AAA: {s}", .{token.text});
                    return token.text[0];
                },
            };
        }

        pub fn acceptSquareSet(gen: *Self, writer: anytype) GenerateError!void {
            var validity_list = std.ArrayListUnmanaged(u8){};
            defer validity_list.deinit(gen.allocator);

            while (true) {
                std.log.info("{any}", .{try gen.core.peek()});
                const thing = try gen.core.accept(comptime ruleset.oneOf(.{ .@"]", .@"\"", .@"'", .@"-", .@"!", .@"/", .@"+", .@"*", .@"?", .@".", .numeric_escape_sequence, .alpha_escape_sequence, .square_bracket_element, .identifier }));
                // std.log.info("{any}", .{thing.type});
                switch (thing.type) {
                    .@"-" => {
                        if (validity_list.items.len == 0) {
                            try validity_list.append(gen.allocator, '-');
                            continue;
                        }
                        const start = validity_list.pop();
                        const end_tok = try gen.core.accept(comptime ruleset.oneOf(.{ .numeric_escape_sequence, .alpha_escape_sequence, .square_bracket_element, .identifier }));
                        const end = vliToU8(end_tok);

                        if (end_tok.type == .identifier and end_tok.text.len > 1) try validity_list.appendSlice(gen.allocator, end_tok.text[1..]);

                        var i: u8 = start;
                        while (i <= end) : (i += 1) {
                            try validity_list.append(gen.allocator, i);
                        }
                    },
                    .@"]" => {
                        // std.log.info("{d}", .{validity_list.items});
                        // // TODO
                        // try writer.writeAll("try writer.writeByte((&[_]u8{");
                        // for (validity_list.items) |v| try writer.print("{d}, ", .{v});
                        // try writer.print("}})[random.intRangeLessThan(u8, 0, {d})]);", .{validity_list.items.len});
                        try writer.writeAll("try writer.writeByte(\"");
                        try writer.print("{}", .{std.zig.fmtEscapes(validity_list.items)});
                        try writer.print("\"[random.intRangeLessThan(u8, 0, {d})]);", .{validity_list.items.len});
                        return;
                    },
                    .identifier => {
                        try validity_list.appendSlice(gen.allocator, thing.text);
                    },
                    .numeric_escape_sequence, .alpha_escape_sequence, .square_bracket_element, .@"\"", .@"'", .@"!", .@"/", .@"+", .@"*", .@"?", .@"." => try validity_list.append(gen.allocator, vliToU8(thing)),
                    else => unreachable,
                }
            }
        }

        pub fn acceptString(gen: *Self, writer: anytype) GenerateError!void {
            var string = std.ArrayListUnmanaged(u8){};
            defer string.deinit(gen.allocator);

            while (true) {
                // std.log.info("{any}", .{try gen.core.peek()});
                // const thing = try gen.core.accept(comptime ruleset.oneOf(.{ .numeric_escape_sequence, .alpha_escape_sequence, .square_bracket_element, .identifier, .@"'", .@"\"" }));
                const thing = try gen.core.nextToken() orelse return;
                std.log.info("{s}: {s}", .{ thing.text, @tagName(thing.type) });
                switch (thing.type) {
                    .@"'", .@"\"" => {
                        try writer.print("try writer.writeAll(\"{}\");", .{std.zig.fmtEscapes(string.items)});
                        return;
                    },
                    .identifier => {
                        try string.appendSlice(gen.allocator, thing.text);
                    },
                    else => try string.append(gen.allocator, vliToU8(thing)),
                }
            }
        }

        pub fn acceptExpression(gen: *Self, writer: anytype) GenerateError!void {
            const state = gen.core.saveState();
            errdefer gen.core.restoreState(state);

            var slash_bufs = std.ArrayListUnmanaged(std.ArrayList(u8)){};
            defer slash_bufs.deinit(gen.allocator);

            try slash_bufs.append(gen.allocator, std.ArrayList(u8).init(gen.allocator));

            var slash_buf = &slash_bufs.items[slash_bufs.items.len - 1];
            var negate = false;

            while (true) {
                var pre_state = gen.core.saveState();
                const thing = gen.core.accept(comptime ruleset.oneOf(.{ .@"identifier", .@"(", .@"[", .@"!", .@".", .@"'", .@"\"" })) catch break;

                var buf = std.ArrayList(u8).init(gen.allocator);

                if (try gen.core.peek()) |c| {
                    switch (c.type) {
                        .@"<-" => {
                            gen.core.restoreState(pre_state);
                            break;
                        },
                        else => {},
                    }
                }

                switch (thing.type) {
                    .identifier => {
                        try buf.writer().print("try {s}(writer, random);", .{std.zig.fmtId(thing.text)});
                    },
                    .@"(" => {
                        try buf.writer().writeByte('{');
                        try gen.acceptExpression(buf.writer());
                        _ = try gen.core.accept(comptime ruleset.is(.@")"));
                        try buf.writer().writeByte('}');
                    },
                    .@"[" => {
                        try gen.acceptSquareSet(buf.writer());
                    },
                    .@"!" => {
                        negate = true;
                    },
                    .@"." => {
                        try buf.writer().writeAll("try writer.writeByte(random.intRangeLessThan(u8, 32, 126));");
                    },
                    .@"\"", .@"'" => {
                        try gen.acceptString(buf.writer());
                    },
                    else => unreachable,
                }

                if (try gen.core.peek()) |c| {
                    if (!negate) {
                        switch (c.type) {
                            .@"?" => {
                                try slash_buf.writer().writeAll("if (random.boolean()) {");
                                try slash_buf.appendSlice(buf.items);
                                try slash_buf.writer().writeAll("}");
                            },
                            .@"*" => {
                                try slash_buf.writer().print("var id{d}: usize = 0; const b{d} = random.intRangeAtMost(usize, 0, 128); while (id{d} < b{d}) : (id{d} += 1) {{", .{ gen.ii, gen.ii, gen.ii, gen.ii, gen.ii });
                                gen.ii += 1;
                                try slash_buf.appendSlice(buf.items);
                                try slash_buf.writer().writeAll("}");
                            },
                            .@"+" => {
                                try slash_buf.writer().print("var id{d}: usize = 0; const b{d} = random.intRangeAtMost(usize, 0, 128); while (id{d} < b{d}) : (id{d} += 1) {{", .{ gen.ii, gen.ii, gen.ii, gen.ii, gen.ii });
                                gen.ii += 1;
                                try slash_buf.appendSlice(buf.items);
                                try slash_buf.writer().writeAll("}");
                            },
                            else => {
                                try slash_buf.appendSlice(buf.items);
                            },
                        }
                    }
                }

                _ = gen.core.accept(comptime ruleset.is(.@"?")) catch {};
                _ = gen.core.accept(comptime ruleset.is(.@"*")) catch {};
                _ = gen.core.accept(comptime ruleset.is(.@"+")) catch {};

                if (try gen.core.peek()) |c| {
                    switch (c.type) {
                        .@"/" => {
                            _ = try gen.core.accept(comptime ruleset.is(.@"/"));
                            if (!negate) {
                                try slash_bufs.append(gen.allocator, std.ArrayList(u8).init(gen.allocator));
                                slash_buf = &slash_bufs.items[slash_bufs.items.len - 1];
                            }
                        },
                        else => {},
                    }
                }

                if (negate and thing.type != .@"!") negate = false;
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
    };
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var out = try std.fs.cwd().createFile("out.zig", .{});
    defer out.close();

    var grammar = try std.fs.cwd().openFile("grammar.y", .{});
    defer grammar.close();

    var data = try grammar.readToEndAlloc(allocator, 50_000);
    defer allocator.free(data);

    var writer = out.writer();
    try writer.writeAll("const Random = @import(\"std\").rand.Random;\n");
    try PegGen(std.fs.File.Writer).generate(allocator, writer, data);
}
