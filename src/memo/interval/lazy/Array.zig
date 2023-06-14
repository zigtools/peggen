// An Array is another implementation of the interval.Set backed by an array
// rather than an AVL tree. This implementation is naive and ineffecient, but
// provides a good point of comparison for benchmarking and testing.
slots: std.ArrayListUnmanaged(slot) = .{},

const std = @import("std");
const mem = std.mem;
const Array = @This();
const tree = @import("tree.zig");
const Interval = tree.Interval;
pub const IValue = tree.IValue;

const slot = struct {
    ivalue: IValue,
    id: usize,
};

pub fn deinit(a: *Array, allocator: mem.Allocator) void {
    a.slots.deinit(allocator);
}

pub fn pos(iv: IValue) usize {
    return iv.interval.Low;
}

pub fn findLargest(a: Array, id: usize, pos_: isize) ?IValue {
    var max: isize = 0;
    var maxi: isize = -1;
    for (a.slots.items, 0..) |in, i| {
        if (in.ivalue.interval.low == pos_ and in.id == id and in.ivalue.interval.high > max) {
            maxi = @intCast(isize, i);
            max = in.ivalue.interval.high;
        }
    }
    if (maxi == -1 or maxi >= a.slots.items.len) {
        return null;
    }

    return a.slots.items[@bitCast(usize, maxi)].ivalue;
}

pub fn add(a: *Array, allocator: mem.Allocator, id: usize, low: isize, high: isize, val: usize) !tree.Value {
    const iv = IValue{
        .interval = Interval.init(low, high),
        .value = val,
    };
    try a.slots.append(allocator, slot{
        .id = id,
        .ivalue = iv,
    });
    return tree.Value.init(iv);
}

pub fn removeAndShift(a: *Array, low: isize, high: isize, amt: isize) void {
    {
        var i: usize = 0;
        while (i < a.slots.items.len) {
            if (a.slots.items[i].ivalue.interval.overlaps(low, high)) {
                a.slots.items[i] = a.slots.items[a.slots.items.len - 1];
                a.slots.items.len -= 1;
            } else {
                i += 1;
            }
        }
    }

    if (amt == 0) return;

    for (0..a.slots.items.len) |i| {
        if (a.slots.items[i].ivalue.interval.low >= low) {
            a.slots.items[i].ivalue.interval = a.slots.items[i].ivalue.interval.shift(amt);
        }
    }
}

pub fn size(a: *Array) usize {
    return a.slots.items.len;
}
