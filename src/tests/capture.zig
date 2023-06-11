const std = @import("std");
const p = @import("../pattern.zig");
const Vm = @import("../Vm.zig");
const memo = @import("../memo.zig");

const testing = std.testing;
const talloc = testing.allocator;

test "Captures" {
    const digit = 0;
    const num = 1;

    var pat = try p.ZeroOrMore(p.Memo(p.Group(&.{
        p.Cap(p.OneOrMore(
            p.Cap(p.CharRange('0', '9'), digit),
        ), num),
        p.Optional(p.String(" ")),
    }))).normalize(talloc);
    defer pat.deinit(talloc);

    var prog = try pat.compileAndOptimize(talloc);
    defer prog.deinit(talloc);
    var code = try Vm.encode(talloc, prog);
    defer code.deinit();

    var fbs = std.io.fixedBufferStream("12 34 56 78 9");
    var memotbl = memo.Table{ .none = {} };
    var arena = std.heap.ArenaAllocator.init(talloc);
    defer arena.deinit();
    const res = try code.exec(fbs.seekableStream(), &memotbl, arena.allocator());

    const expect = [_][2]usize{
        .{ 0, 2 },
        .{ 3, 2 },
        .{ 6, 2 },
        .{ 9, 2 },
        .{ 12, 1 },
    };

    const ast = res[2];
    var buf: [10]memo.Capture.ChildIterator = undefined;
    var it = ast.?.childIterator(&buf);
    var i: usize = 0;
    while (try it.next()) |ch| {
        // std.debug.print("i={} expected={any} actual={{{}, {}}}\n", .{ i, expect[i], ch.start(), ch.length });
        if (expect[i][0] != ch.start() or expect[i][1] != ch.length) {
            std.debug.print(
                "expected ch.start() {} == {} and ch.length {} == {} \n",
                .{ ch.start(), expect[i][0], ch.length, expect[i][1] },
            );
            return error.TestUnexpectedResult;
        }
        i += 1;
    }
    try testing.expectEqual(expect.len, i);
}
