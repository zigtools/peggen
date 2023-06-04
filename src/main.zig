const std = @import("std");
const Stream = @import("Stream.zig");
const PegParser = @import("PegParser.zig");
const Grammar = PegParser.Grammar;
const Expression = PegParser.Expression;

pub fn generate(result: Grammar, writer: anytype) !void {
    try writer.writeAll(
        \\pub fn Parser(comptime ParserGenerator: type) type {
        \\return struct{
    );

    for (result.rules.items) |rule| {
        try writer.print("pub const {} = struct {{pub usingnamespace {};}};\n\n", .{
            std.zig.fmtId(rule.identifier),
            Expression.Formatter{ .expr = rule.expression, .rule = rule.identifier },
        });
    }

    try writer.writeAll(
        \\};}
    );
}

pub fn generateFormatted(allocator: std.mem.Allocator, result: Grammar) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);

    try generate(result, buffer.writer());
    // return buffer.toOwnedSlice();

    const with_sentinel = try buffer.toOwnedSliceSentinel(0);
    defer allocator.free(with_sentinel);

    const tree = try std.zig.Ast.parse(allocator, with_sentinel, .zig);
    return tree.render(allocator);
}

fn chopArg(args: *[]const []const u8) ?[]const u8 {
    if (args.len == 0) return null;
    defer args.* = args.*[1..];
    return args.*[0];
}

fn usage(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print(fmt ++ "\n", args);
    std.debug.print("usage: $ peggen <mode> <file.peg> <?outfile.zig> \n", .{});
    std.debug.print("  <mode>: {s}\n", .{std.meta.fieldNames(Mode)});
    std.os.exit(1);
}

pub const std_options = struct {
    pub const log_level = .err;
};

const Mode = enum { gen, print, parse };

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = arena.allocator();
    var args = try std.process.argsAlloc(alloc);
    _ = chopArg(&args).?;
    const mode_raw = chopArg(&args) orelse
        usage("missing argument: <mode>", .{});
    const mode = std.meta.stringToEnum(Mode, mode_raw) orelse
        usage("invalid mode: '{s}'", .{mode_raw});
    const pegfilename = chopArg(&args) orelse
        usage("missing argument: <file.peg>", .{});

    const pegfile = try std.fs.cwd().openFile(pegfilename, .{});
    defer pegfile.close();
    const input = try pegfile.readToEndAlloc(alloc, std.math.maxInt(u32));
    var output = try alloc.alloc(u8, input.len);

    switch (mode) {
        .print => {
            var pegparser = PegParser.init(alloc, input, output, pegfilename);
            const g = try pegparser.parseGrammar();
            for (g.rules.items) |rule|
                std.debug.print("{s} <- {}\n", .{ rule.identifier, rule.expression });
        },
        .parse => {
            const generated_parser = @import("../out.zig");
            const Pg = @import("ParserGenerator.zig");
            const p = generated_parser.Parser(Pg);
            var stream = Stream.init(input, output);
            var ctx = Pg.Context{ .file_path = pegfilename };
            ctx.flags.insert(.print_errors);
            const g = try Pg.parse(p.Grammar, &stream, &ctx);
            std.debug.print("g={s}\n", .{g});
        },
        .gen => {
            const outfilename = chopArg(&args) orelse
                usage("missing argument: <?outfile.zig>", .{});
            var pegparser = PegParser.init(alloc, input, output, pegfilename);
            const g = try pegparser.parseGrammar();
            const outfile = try std.fs.cwd().createFile(outfilename, .{});
            defer outfile.close();
            const generated = try generateFormatted(alloc, g);
            try outfile.writeAll(generated);
        },
    }
}
