const std = @import("std");
const Array = @import("../memo/interval/lazy/Array.zig");
const lazylogtree = @import("../memo/interval/lazylog/tree.zig");
const lazytree = @import("../memo/interval/lazy/tree.zig");
const Tree = lazylogtree.Tree;

var prng = std.rand.DefaultPrng.init(0);
const rand = prng.random();

fn randrange(max: isize) [2]isize {
    const low = rand.intRangeAtMost(isize, 0, max);
    const high = low + rand.intRangeAtMost(isize, 0, 1000);
    return if (low == high)
        .{ low, low + 1 }
    else
        .{ low, high };
}

fn randint(min: isize, max: isize) isize {
    return rand.intRangeAtMost(isize, 0, max - min) + min;
}

const t = std.testing;
const talloc = t.allocator;

const Op = enum { add, find, remove_and_shift, pos };

test {
    try main();
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = arena.allocator();
    // const alloc = std.heap.c_allocator;
    // const alloc = t.allocator;
    var it = Array{};
    defer it.deinit(alloc);
    var ia = Tree{};
    defer ia.deinit(alloc);
    const nops = 300000;
    const maxidx = 10;
    const maxid = 10;
    const maxshamt = 50;

    var pt: lazytree.Value = .none;
    var pa: lazytree.Value = .none;
    var length: isize = 0;
    var haspt: bool = false;
    // t.log_level = .info;

    for (0..nops) |i| {
        const op = rand.enumValue(Op);
        switch (op) {
            .add => {
                const id = rand.intRangeAtMost(u8, 0, maxid);
                const lowhigh = randrange(maxidx);
                const low = lowhigh[0];
                const high = lowhigh[1];

                pt = try it.add(alloc, id, low, high, i);
                const iv = try alloc.create(lazytree.IValue);
                iv.* = .{ .value = i };
                pa = try ia.add(alloc, id, low, high, lazytree.Value.init(iv));
                std.log.debug("=== {} op={s} {}:[{},{}] pt/pa.pos()=[{},{}]", .{ i, @tagName(op), id, low, high, pt.pos(), pa.pos() });
                length = high - low;
                haspt = true;
            },
            .find => {
                const id = rand.intRangeAtMost(usize, 0, maxid);
                const pos = rand.intRangeAtMost(isize, 0, maxidx);
                const vt = it.findLargest(id, pos);
                const va = ia.findLargest(id, pos);
                // std.log.debug("=== {} op={s} {}:{} va={?}/{?} vt={?}/{?}", .{ i, @tagName(op), id, pos, va, vaex, vt, vtex });
                if (vt == null and va == null)
                    continue;
                std.log.debug("i={} vt={?} va={?}", .{ i, vt, va });
                if ((vt == null) != (va == null)) {
                    std.log.err("find2 i={} ({}, {}): {?} != {?}", .{ i, id, pos, vt, va });
                    std.debug.print("it=\n{}\n", .{it});
                    std.debug.print("ia=\n{}\n", .{ia});
                    return error.TestUnexpectedResult;
                }
                if (vt.?.value != va.?.ivalue.value) {
                    std.log.err("find3 i={} ({}, {}): {?} != {?}", .{ i, id, pos, vt.?.value, va.?.ivalue.value });
                    return error.TestUnexpectedResult;
                }
            },
            .remove_and_shift => {
                const lowhigh = randrange(maxidx);
                const amt = randint(-maxshamt, maxshamt);
                const low = lowhigh[0];
                const high = lowhigh[1];

                if (haspt) {
                    const ptpos = pt.pos();
                    if ((lazytree.Interval{ .low = low, .high = high })
                        .overlaps(ptpos, ptpos + length))
                    {
                        haspt = false;
                    }
                }
                std.log.debug("=== {} op={s} {}:[{},{}]", .{ i, @tagName(op), amt, low, high });
                it.removeAndShift(low, high, amt);
                try ia.removeAndShift(alloc, low, high, amt);
            },
            .pos => {
                std.log.debug("=== {} op={s}", .{ i, @tagName(op) });
                const post = pt.pos();
                const posa = pa.pos();
                if (haspt) {
                    // std.log.debug("pt={} pa={}", .{ pt, pa });

                    if (post != posa) {
                        // if (post != postex) {
                        std.log.err("i={} post {} != posa {}", .{ i, post, posa });
                        return error.TestUnexpectedResult;
                    }
                }
            },
        }
    }
}
