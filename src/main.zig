const std = @import("std");
const Stream = @import("Stream.zig");
const PegParser = @import("PegParser.zig");
const Grammar = PegParser.Grammar;
const Expression = PegParser.Expression;
const pattern = @import("pattern.zig");
const Vm = @import("Vm.zig");
const memo = @import("memo.zig");

pub fn generate(result: Grammar, writer: anytype) !void {
    _ = try writer.write(
        \\const std = @import("std");
        \\pub inline fn Rules(comptime ParserGenerator: type, comptime options: struct{eval_branch_quota: usize = 1000},) []const ParserGenerator.Rule {
        \\@setEvalBranchQuota(options.eval_branch_quota);
        \\return comptime &.{
        \\
    );
    for (result.rules.items) |rule| {
        try writer.print(".{{\"{}\", {} }},\n", .{
            std.zig.fmtId(rule.identifier),
            Expression.Formatter{ .expr = rule.expression, .rule = rule.identifier },
        });
    }
    _ = try writer.write(
        \\}; }
        \\
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
    std.debug.print("usage: $ peggen <mode> <file> <?outfile> <?start>\n", .{});
    std.debug.print("  <mode>: {s}\n", .{std.meta.fieldNames(Mode)});
    std.debug.print("usage examples:\n", .{});
    std.debug.print("  $ peggen print file.peg # parse and print 'file.peg'\n", .{});
    std.debug.print("  $ peggen gen   file.peg out.zig # generate a grammar from 'file.peg' and save to 'out.zig'\n", .{});
    std.debug.print("  $ peggen parse file start # parse 'file' using the grammar in out.zig and starting rule 'start' \n", .{});
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
    const filename = chopArg(&args) orelse
        usage("missing argument: <file>", .{});

    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    const input = try file.readToEndAlloc(alloc, std.math.maxInt(u32));
    var output = try alloc.alloc(u8, input.len);

    switch (mode) {
        .print => {
            var pegparser = PegParser.init(alloc, input, output, filename);
            const g = try pegparser.parseGrammar();
            for (g.rules.items) |rule|
                std.debug.print("{s} <- {}\n", .{ rule.identifier, rule.expression });
        },
        .parse => {
            const generated = @import("../out.zig");
            const rules = comptime generated.Rules(pattern, .{ .eval_branch_quota = 2000 });
            const start = chopArg(&args) orelse
                usage("missing argument: <?start>", .{});
            var g = try pattern.Pattern.rulesToGrammar(alloc, rules, start);
            const prog = try g.compileAndOptimize(alloc);
            std.debug.print("prog.len={}\n", .{prog.items.len});
            // for (prog.items) |insn| {
            //     std.debug.print("{}\n", .{insn});
            // }

            var vm = try Vm.encode(alloc, prog);
            var memotbl: memo.Table = .none;
            // var memotbl = memo.Table{ .tree = .{ .tree = .{ .root = null } } };
            var cap_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            const result = try vm.exec(file.seekableStream(), &memotbl, cap_arena.allocator());
            std.debug.print("errs: {any}\n", .{result[3].items});
        },
        .gen => {
            const outfilename = chopArg(&args) orelse
                usage("missing argument: <?outfile.zig>", .{});
            var pegparser = PegParser.init(alloc, input, output, filename);
            const g = try pegparser.parseGrammar();
            const outfile = try std.fs.cwd().createFile(outfilename, .{});
            defer outfile.close();
            try outfile.writeAll(try generateFormatted(alloc, g));
        },
    }
}
