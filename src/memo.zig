const std = @import("std");
const mem = std.mem;
const tree = @import("tree.zig");
const Tree = tree.Tree;
const Node = tree.Node;
const LazyNode = tree.LazyNode;

/// An Edit represents a modification to the subject string where the interval
/// [Start, End) is modified to be Len bytes. If Len = 0, this is equivalent
/// to deleting the interval, and if Start = End this is an insertion.
pub const Edit = struct {
    start: usize,
    end: usize,
    len: usize,
};

// An Entry represents a memoized parse result. It stores the non-terminal
// memoized, the start position of the parse result, the length, and the number
// of characters examined to make the parse determination. If the length is -1,
// the non-terminal failed to match at this location (but still may have
// examined a non-zero number of characters).
pub const Entry = struct {
    length: isize,
    examined: usize,
    count: usize,
    captures: Capture.List = .{},
    pos: usize,

    pub fn deinit(e: *Entry, allocator: mem.Allocator) void {
        for (e.captures.items) |*capt| capt.deinit(allocator);
        e.captures.deinit(allocator);
    }

    fn setPos(e: *Entry, allocator: mem.Allocator, pos: Pos) !void {
        e.pos = try pos.pos(allocator);
        for (e.captures.items) |*cap|
            cap.setMEnt(e);
    }
};

pub const Capture = struct {
    id: u32,
    type: Type,
    off: usize,
    length: usize,
    ment: ?*Entry = null,
    children: List = .{},

    pub const List = std.ArrayListUnmanaged(Capture);
    pub const Type = enum { node, dummy };

    pub fn deinit(capt: *Capture, allocator: mem.Allocator) void {
        if (capt.ment) |e| e.deinit(allocator);
        for (capt.children.items) |*c| c.deinit(allocator);
    }

    pub fn initNode(id: usize, offset: usize, length: usize, children: List) Capture {
        return Capture{
            .id = @intCast(u32, id),
            .type = .node,
            .off = offset,
            .length = length,
            .children = children,
        };
    }

    pub fn initDummy(offset: usize, length: usize, children: List) Capture {
        return .{
            .id = 0,
            .type = .dummy,
            .off = offset,
            .length = length,
            .children = children,
        };
    }

    pub fn setMEnt(c: *Capture, e: *Entry) void {
        if (c.ment != null) return;

        c.ment = e;
        c.off = c.off - e.pos;

        for (c.children.items) |*child|
            child.setMEnt(e);
    }
    pub const ChildIterator = struct {
        capt: Capture,
        buf: []ChildIterator,
        depth: usize = 0,

        /// returns only children which have type='node' entries, no
        /// type='dummy' entries which seem to serve only as parent nodes
        pub fn next(iter: *ChildIterator) !?Capture {
            // std.debug.print("iter.next() iter depth={} capt={} children={any}\n", .{ iter.depth, iter.capt, iter.capt.children.items });
            if (iter.capt.children.items.len == 0) {
                if (iter.depth == 0)
                    return null
                else {
                    iter.depth -= 1;
                    iter.* = iter.buf[iter.depth];
                    iter.capt.children.items = iter.capt.children.items[1..];
                    return iter.next();
                }
            }

            const ch = iter.capt.children.items[0];
            // std.debug.print("iter.next() ch=.{s}\n", .{@tagName(ch.type)});
            if (ch.type == .node) {
                iter.capt.children.items = iter.capt.children.items[1..];
                return ch;
            }

            std.debug.assert(ch.type == .dummy);
            if (iter.depth >= iter.buf.len) return error.OutOfMemory;
            // save self in puffer and descend into children
            iter.buf[iter.depth] = iter.*;
            const depth = iter.depth + 1;
            iter.* = ch.childIterator(iter.buf);
            iter.depth = depth;
            return iter.next();
        }
    };

    /// an iterator over a Capture and its children. 'buf.len' should be >=
    /// to the maximum child depth.
    pub fn childIterator(
        capt: Capture,
        buf: []ChildIterator,
    ) ChildIterator {
        return .{ .capt = capt, .buf = buf };
    }

    pub fn start(c: Capture) usize {
        if (c.ment) |ent| {
            return ent.pos + c.off;
        }
        return c.off;
    }

    pub fn format(c: Capture, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("type=.{s} id={}", .{ @tagName(c.type), c.id });
        if (c.type == .node)
            try writer.print(" range=[{},{}]", .{ c.start(), c.length });
    }
};

// A key stores the start position of an interval, and a unique ID if you would
// like to store multiple intervals starting from the same position. The key is
// used for uniquely identifying a particular interval when searching or
// removing from the tree.
pub const Key = struct {
    pos: usize,
    id: usize,
};

// A Table is an interface for a memoization table data structure. The
// memoization table tracks memoized parse results corresponding to a
// non-terminal parsed at a certain location. The table interface defines the
// ApplyEdit function which is crucial for incremental parsing.
pub const Table = union(enum) {
    tree: *TreeTable,
    none,
    // Get returns the entry associated with the given position and ID. If
    // there are multiple entries with the same ID at that position, the
    // largest entry is returned (determined by matched length).
    pub fn get(t: Table, id: usize, pos: usize) ?*Entry {
        _ = pos;
        _ = id;
        switch (t) {
            .none => return null,
            .tree => unreachable,
        }
    }

    // Put adds a new entry to the table.
    pub fn put(
        t: *Table,
        allocator: mem.Allocator,
        id: usize,
        start: usize,
        length: isize,
        examined: usize,
        count: usize,
        captures: Capture.List,
    ) !void {
        switch (t.*) {
            .none => {},
            .tree => |tt| try tt.put(allocator, id, start, length, examined, count, captures),
        }
    }

    // ApplyEdit updates the table as necessary when an edit occurs. This
    // operation invalidates all entries within the range of the edit and
    // shifts entries that are to the right of the edit as necessary.
    pub fn applyEdit(t: *Table, allocator: mem.Allocator, edit: Edit) !void {
        switch (t.*) {
            .none => {},
            .tree => |tt| try tt.applyEdit(allocator, edit),
        }
    }
};

pub const Interval = struct {
    low: usize,
    high: usize,

    pub fn init(low: usize, high: usize) Interval {
        return .{ .low = low, .high = high };
    }

    pub fn overlaps(i: Interval, low: usize, high: usize) bool {
        return i.low < high and i.high > low;
    }

    pub fn shift(i: Interval, amt: usize) Interval {
        return Interval{
            .low = i.low + amt,
            .high = i.high + amt,
        };
    }

    pub fn len(i: Interval) usize {
        return i.high - i.low;
    }
};

pub const LazyInterval = struct {
    ins: std.ArrayListUnmanaged(LazyInterval.Data) = .{},
    n: ?*LazyNode = null,

    pub const Data = struct {
        low: usize,
        high: usize,
        value: ?*Entry = null,
        pub fn init(low: usize, h: usize) LazyInterval.Data {
            return .{ .low = low, .high = h };
        }
        pub fn overlaps(i: LazyInterval.Data, low: usize, h: usize) bool {
            return i.low < h and i.high > low;
        }
    };

    pub fn deinit(i: *LazyInterval, allocator: mem.Allocator) void {
        i.ins.deinit(allocator);
    }

    pub fn pos(i: LazyInterval, allocator: mem.Allocator) usize {
        _ = allocator;
        LazyNode.applyShifts(i.n);
        return i.n.key.pos;
    }

    pub fn high(i: LazyInterval) usize {
        var h: usize = 0;
        for (i.ins.items) |in| {
            if (in.high > h) h = in.high;
        }
        return h;
    }

    pub fn shift(i: *LazyInterval, amt: usize) void {
        for (0..i.ins.items.len) |j| {
            i.ins.items[j].low += amt;
            i.ins.items[j].high += amt;
        }
    }
};

pub const IValues = struct {
    ivs: std.ArrayListUnmanaged(IValue) = .{},
    node: ?*Node,

    pub fn deinit(ivs: *IValues, allocator: mem.Allocator) void {
        ivs.ivs.deinit(allocator);
    }
};

pub const IValue = struct {
    interval: Interval = Interval.init(0, 0),
    value: usize,

    pub fn format(n: IValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("{}", .{n.interval.Low});
    }
};

pub const Pos = union(enum) {
    ivalue: IValue,
    ivalues: IValues,
    lazy_interval: LazyInterval,
    none,

    pub fn init(p: anytype) Pos {
        const P = @TypeOf(p);
        return switch (P) {
            IValue => .{ .ivalue = p },
            IValues => .{ .ivalues = p },
            LazyInterval => .{ .lazy_interval = p },
            else => @compileError("type '" ++ @typeName(P) ++ "' not supported."),
        };
    }

    pub fn pos(p: @This(), allocator: mem.Allocator) !usize {
        switch (p) {
            .ivalue => |iv| return iv.interval.low,
            .ivalues => |iv| {
                try Node.applyAllShifts(iv.node, allocator);
                return if (iv.node) |n| n.key.pos else 0;
            },
            .lazy_interval => |i| {
                LazyNode.applyShifts(i.n);
                return i.n.?.key.pos;
            },
            .none => unreachable,
        }
    }
};

// An interval map is a key-value data structure that maps intervals to
// values.  Every value is associated with an interval [low, high) and an id.
// Values may be looked up, added, removed, and queried for overlapping
// intervals. The tree also supports efficient shifting of intervals via
// a lazy shift propagation mechanism.
pub fn Map(comptime Value: type) type {
    return union(enum) {
        pub const Self = @This();

        foo,
        bar,

        // Returns the value associated with the largest interval at (id, pos).
        pub fn findLargest(self: *Self, allocator: mem.Allocator, id: usize, pos: usize) Value {
            _ = pos;
            _ = id;
            _ = allocator;
            _ = self;
            unreachable;
        }

        // Adds a new value with 'id' and interval [low, high). Returns a value
        // that can be used to locate the inserted value even after shifts have
        // occurred (you may want to associate the Pos with your value).
        pub fn add(self: *Self, allocator: mem.Allocator, id: usize, low: usize, high: usize, val: Value) Pos {
            _ = val;
            _ = high;
            _ = low;
            _ = id;
            _ = allocator;
            _ = self;
            unreachable;
        }
        // Removes all values with intervals that overlap [low, high) and then
        // performs a shift of size amt at idx.
        pub fn removeAndShift(self: *Self, low: usize, high: usize, amt: usize) void {
            _ = amt;
            _ = high;
            _ = low;
            _ = self;
            unreachable;
        }
    };
}

pub const TreeTable = struct {
    map: Map(Entry) = .{ .foo = {} },
    tree: Tree,
    threshold: usize = 0,
    lock: std.Thread.Mutex = .{},

    pub fn applyEdit(t: *TreeTable, allocator: mem.Allocator, e: Edit) !void {
        const low = e.start;
        var high = e.end;
        high += @boolToInt(low == high);

        const amt = e.len - (e.end - e.start);

        t.lock.lock();
        try t.tree.removeAndShift(allocator, low, high, amt);
        t.lock.unlock();
    }

    pub fn put(
        t: *TreeTable,
        allocator: mem.Allocator,
        id: usize,
        start: usize,
        length: isize,
        examined_: usize,
        count: usize,
        capt: Capture.List,
    ) !void {
        if (examined_ < t.threshold or length == 0)
            return;

        const examined = @max(examined_, @intCast(usize, length));

        var e = try allocator.create(Entry);
        e.* = .{
            .length = length,
            .examined = examined,
            .count = count,
            .captures = capt,
            .pos = 0,
        };
        t.lock.lock();
        try e.setPos(allocator, try t.add(allocator, id, start, start + examined, e));
        t.lock.unlock();
    }

    // Adds the given interval to the tree. An id should also be given to the
    // interval to uniquely identify it if any other intervals begin at the same
    // location.
    pub fn add(t: *TreeTable, allocator: mem.Allocator, id: usize, low: usize, high: usize, value: *Entry) !Pos {
        const node_loc = try LazyNode.add(
            t.tree.root,
            allocator,
            &t.tree,
            .{ .pos = low, .id = id },
            .{ .low = low, .high = high, .value = value },
        );
        t.tree.root = node_loc[0];
        return .{ .lazy_interval = node_loc[1] };
    }
};
