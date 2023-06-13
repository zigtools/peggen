const std = @import("std");
const mem = std.mem;
const lazylogtree = @import("../lazylog/tree.zig");
const LazyInterval = lazylogtree.LazyInterval;
const memotree = @import("../../tree.zig");

pub const Interval = struct {
    low: usize,
    high: usize,

    pub fn init(low: usize, high: usize) Interval {
        return .{ .low = low, .high = high };
    }

    pub fn overlaps(i: Interval, low: usize, high: usize) bool {
        return i.low < high and i.high > low;
    }

    pub fn shift(i: Interval, amt: isize) Interval {
        return Interval{
            .low = @intCast(usize, @bitCast(isize, i.low) + amt),
            .high = @intCast(usize, @bitCast(isize, i.high) + amt),
        };
    }

    pub fn len(i: Interval) usize {
        return i.high - i.low;
    }
};

pub const IValue = struct {
    interval: Interval = Interval.init(0, 0),
    value: usize,

    pub fn format(n: IValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("{}", .{n.interval.low});
    }
};

pub const Value = union(enum) {
    ivalue: IValue,
    // ivalues: IValues,
    lazy_interval: LazyInterval,
    entry: *memotree.Entry,
    none,

    pub fn init(p: anytype) Value {
        const P = @TypeOf(p);
        return switch (P) {
            IValue => .{ .ivalue = p },
            // IValues => .{ .ivalues = p },
            LazyInterval => .{ .lazy_interval = p },
            *memotree.Entry => .{ .entry = p },
            else => @compileError("type '" ++ @typeName(P) ++ "' not supported."),
        };
    }

    pub fn pos(p: @This(), allocator: mem.Allocator) !usize {
        _ = allocator;
        switch (p) {
            .ivalue => |iv| return iv.interval.low,
            // .ivalues => |iv| {
            //     try Node.applyAllShifts(iv.node, allocator);
            //     return if (iv.node) |n| n.key.pos else 0;
            // },
            .entry => unreachable,
            .lazy_interval => |i| {
                lazylogtree.Node.applyShifts(i.n);
                return i.n.?.key.pos;
            },
            .none => unreachable,
        }
    }

    pub fn high(p: @This()) usize {
        switch (p) {
            .ivalue => |iv| return iv.interval.high,
            // .ivalues => |iv| {
            //     try Node.applyAllShifts(iv.node, allocator);
            //     return if (iv.node) |n| n.key.pos else 0;
            // },
            .lazy_interval => |i| {
                return i.high();
            },
            .entry => unreachable,
            .none => unreachable,
        }
    }

    pub fn len(p: @This()) usize {
        switch (p) {
            .ivalue => |iv| return iv.interval.high - iv.interval.low,
            // .ivalues => |iv| {
            //     try Node.applyAllShifts(iv.node, allocator);
            //     return if (iv.node) |n| n.key.pos else 0;
            // },
            .entry => unreachable,
            .lazy_interval => |i| return i.len(),
            .none => unreachable,
        }
    }

    pub fn format(v: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("Value{{.{s}", .{@tagName(v)});
        switch (v) {
            inline else => |payload| try writer.print("={}}}", .{payload}),
        }
    }
};
