const std = @import("std");
const p = @import("../pattern.zig");
const Vm = @import("../Vm.zig");
const memo = @import("../memo/tree.zig");

const testing = std.testing;
const talloc = testing.allocator;

test "Captures" {
    const digit = 0;
    const num = 1;
    const alloc = talloc;
    // var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 20 }){};
    // defer _ = gpa.deinit();
    // const alloc = gpa.allocator();

    var pat = try p.ZeroOrMore(p.Memo(p.Group(&.{
        p.Cap(p.OneOrMore(
            p.Cap(p.CharRange('0', '9'), digit),
        ), num),
        p.Optional(p.String(" ")),
    }))).normalize(alloc);
    defer pat.deinit(alloc);

    var prog = try pat.compileAndOptimize(alloc);
    defer prog.deinit(alloc);
    var code = try Vm.encode(alloc, prog);
    defer code.deinit();

    var fbs = std.io.fixedBufferStream("12 34 56 78 9");
    var memotbl = memo.Table{ .none = {} };
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const res = try code.exec(fbs.seekableStream(), &memotbl, arena.allocator());

    const expected = [_][2]usize{
        .{ 0, 2 },
        .{ 3, 2 },
        .{ 6, 2 },
        .{ 9, 2 },
        .{ 12, 1 },
    };

    var ast = res[2] orelse return error.TesUnexpectedResult;
    var buf: [10]memo.Capture.ChildIterator = undefined;
    var it = ast.childIterator(&buf);
    var i: usize = 0;
    while (try it.next()) |ch| {
        // std.debug.print("i={} expected={any} actual={{{}, {}}}\n", .{ i, expect[i], ch.start(), ch.length });
        if (expected[i][0] != ch.start() or expected[i][1] != ch.length) {
            std.debug.print(
                "expected ch.start() {} == {} and ch.length {} == {} \n",
                .{ ch.start(), expected[i][0], ch.length, expected[i][1] },
            );
            return error.TestUnexpectedResult;
        }
        i += 1;
    }
    try testing.expectEqual(expected.len, i);
}
