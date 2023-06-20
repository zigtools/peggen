const std = @import("std");
const mem = std.mem;
const lazytree = @import("interval/lazy/tree.zig");
const Value = lazytree.Value;
const lazylogtree = @import("interval/lazylog/tree.zig");
const Tree = lazylogtree.Tree;
const Node = lazylogtree.Node;

/// An Edit represents a modification to the subject string where the interval
/// [Start, End) is modified to be Len bytes. If Len = 0, this is equivalent
/// to deleting the interval, and if Start = End this is an insertion.
pub const Edit = struct {
    start: isize,
    end: isize,
    len: isize,
};

// An Entry represents a memoized parse result. It stores the non-terminal
// memoized, the start position of the parse result, the length, and the number
// of characters examined to make the parse determination. If the length is -1,
// the non-terminal failed to match at this location (but still may have
// examined a non-zero number of characters).
pub const Entry = struct {
    length: isize,
    examined: isize,
    count: usize,
    captures: Capture.List = .{},
    pos: isize,

    pub fn deinit(e: *Entry, allocator: mem.Allocator) void {
        for (e.captures.items) |*capt| capt.deinit(allocator);
        e.captures.deinit(allocator);
    }

    fn setPos(e: *Entry, pos: Value) void {
        e.pos = pos.pos();
        for (e.captures.items) |*cap|
            cap.setMEnt(e);
    }
};

pub const Capture = struct {
    id: u32,
    type: Type,
    off: isize,
    length: isize,
    ment: ?*Entry = null,
    children: List = .{},

    pub const List = std.ArrayListUnmanaged(Capture);
    pub const Type = enum { node, dummy };

    pub fn deinit(capt: *Capture, allocator: mem.Allocator) void {
        if (capt.ment) |e| e.deinit(allocator);
        for (capt.children.items) |*c| c.deinit(allocator);
        capt.children.deinit(allocator);
    }

    pub fn initNode(id: usize, offset: isize, length: isize, children: List) Capture {
        return Capture{
            .id = @intCast(u32, id),
            .type = .node,
            .off = offset,
            .length = length,
            .children = children,
        };
    }

    pub fn initDummy(offset: isize, length: isize, children: List) Capture {
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

    pub fn start(c: Capture) isize {
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
    tree: TreeTable,
    none,
    // Get returns the entry associated with the given position and ID. If
    // there are multiple entries with the same ID at that position, the
    // largest entry is returned (determined by matched length).
    pub fn get(t: Table, id: usize, pos: isize) ?*Entry {
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
        start: isize,
        length: isize,
        examined: isize,
        count: usize,
        captures: Capture.List,
    ) !void {
        switch (t.*) {
            .none => {},
            .tree => try t.tree.put(allocator, id, start, length, examined, count, captures),
        }
    }

    // ApplyEdit updates the table as necessary when an edit occurs. This
    // operation invalidates all entries within the range of the edit and
    // shifts entries that are to the right of the edit as necessary.
    pub fn applyEdit(t: *Table, allocator: mem.Allocator, edit: Edit) !void {
        switch (t.*) {
            .none => {},
            .tree => try t.tree.applyEdit(allocator, edit),
        }
    }
};

pub const TreeTable = struct {
    // map: Map(Entry) = .{ .foo = {} },
    tree: Tree,
    threshold: usize = 0,
    lock: std.Thread.Mutex = .{},

    pub fn applyEdit(t: *TreeTable, allocator: mem.Allocator, e: Edit) !void {
        const low = e.start;
        var high = e.end;
        high += @boolToInt(low == high);

        const amt = e.len - (e.end - e.start);

        t.lock.lock();
        try t.tree.removeAndShift(allocator, low, high, @intCast(isize, amt));
        t.lock.unlock();
    }

    pub fn put(
        t: *TreeTable,
        allocator: mem.Allocator,
        id: usize,
        start: isize,
        length: isize,
        examined_: isize,
        count: usize,
        capt: Capture.List,
    ) !void {
        if (examined_ < t.threshold or length == 0)
            return;

        const examined = @max(examined_, length);

        t.lock.lock();
        var entry = try allocator.create(Entry);
        entry.* = .{
            .length = length,
            .examined = examined,
            .count = count,
            .captures = capt,
            .pos = 0,
        };
        const pos = try t.add(allocator, id, start, start + examined, .{ .entry = entry });
        entry.setPos(pos);
        t.lock.unlock();
    }

    // Adds the given interval to the tree. An id should also be given to the
    // interval to uniquely identify it if any other intervals begin at the same
    // location.
    pub fn add(t: *TreeTable, allocator: mem.Allocator, id: usize, low: isize, high: isize, value: Value) !Value {
        const node_loc = try Node.add(
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
