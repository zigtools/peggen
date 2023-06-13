const std = @import("std");
const mem = std.mem;
const lazytree = @import("../lazy/tree.zig");

const Value = lazytree.Value;
pub const Interval = struct {
    low: usize,
    high: usize,
    value: Value,

    pub fn len(i: Interval) usize {
        return i.high - i.low;
    }

    // returns true if i overlaps with the interval [low:high)
    pub fn overlaps(i: Interval, low: usize, high: usize) bool {
        return i.low <= high and i.high >= low;
    }

    pub fn format(i: Interval, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("lazylog.Interval{{[{},{}]:{}", .{ i.low, i.high, i.value });
    }
};

/// A shift of intervals in the tree. The shift starts at idx and moves
/// intervals after idx by amt. Shifts are lazily applied in the tree to avoid
/// linear time costs.
pub const Shift = struct {
    idx: usize,
    amt: isize,
    tstamp: u64,
};

/// ShiftThreshold is the number of shifts to accumulate before applying all
/// shifts.
const ShiftThreshold = -1;

pub const Tree = struct {
    root: ?*Node,
    shifts: std.ArrayListUnmanaged(Shift) = .{},
    tstamp: u64 = 0, // most recent timestamp

    pub inline fn init(allocator: mem.Allocator) !Tree {
        const root = try allocator.create(Node);
        root.* = .{
            .key = Key.init(0, 0),
            .max = 0,
            .interval = .{},
            .height = 0,
            .left = null,
            .right = null,
            .tstamp = 0,
            .tree = undefined,
        };
        var tree = Tree{ .root = root, .tstamp = 0 };
        root.tree = &tree;
        return tree;
    }

    pub fn deinit(t: *Tree, allocator: mem.Allocator) void {
        t.shifts.deinit(allocator);
        Node.deinit(t.root, allocator);
    }

    pub fn format(t: Tree, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("Tree{{ .root={?} .shifts={{", .{t.root});
        for (t.shifts.items, 0..) |sh, i| {
            if (i != 0) _ = try writer.write(", ");
            try writer.print("{}", .{sh});
        }
        _ = try writer.write("} }");
    }

    // Adds the given interval to the tree. An id should also be given to the
    // interval to uniquely identify it if any other intervals begin at the same
    // location.
    pub fn add(t: *Tree, allocator: mem.Allocator, id: usize, low: usize, high: usize, value: Value) !Value {
        std.debug.assert(low < high);
        const r_loc = try Node.add(
            t.root,
            allocator,
            t,
            .{ .pos = low, .id = id },
            .{ .low = low, .high = high, .value = value },
        );
        t.root = r_loc[0];
        return Value.init(r_loc[1]);
    }

    // Search for the interval starting at pos with the given id. Returns null if no
    // such interval exists.
    pub fn findLargest(t: *Tree, allocator: mem.Allocator, id: usize, pos: usize) !?*Value {
        const n = try Node.search(t.root, allocator, Key{
            .pos = pos,
            .id = id,
        }) orelse return null;

        if (n.interval.ins.items.len == 0)
            return null;

        var max: usize = 0;
        for (n.interval.ins.items[1..], 0..) |in, i| {
            if (in.len() > n.interval.ins.items[max].len()) {
                max = i + 1;
            }
        }

        return &n.interval.ins.items[max].value;
    }

    pub fn removeAndShift(t: *Tree, allocator: mem.Allocator, low: usize, high: usize, amt: isize) !void {
        std.debug.assert(low < high);
        const tmp = t.root;
        t.root = try Node.removeOverlaps(tmp, allocator, low, high);
        if (amt != 0) {
            try t.shift(allocator, low, amt);
        }
    }

    // Shift all intervals in the tree after idx by amt. The shift idx should not
    // lie inside an interval. This could conceivably be implemented, but is not
    // currently. If a negative shift is performed, ensure that there is space for
    // all intervals to be shifted left without overlapping with another interval.
    fn shift(t: *Tree, allocator: mem.Allocator, idx: usize, amt: isize) !void {
        if (amt == 0) return;

        t.tstamp += 1;
        try t.shifts.append(allocator, .{
            .idx = idx,
            .amt = amt,
            .tstamp = t.tstamp,
        });
        if (ShiftThreshold != -1 and t.shifts.len >= ShiftThreshold) {
            t.applyAllShifts();
        }
    }

    fn applyAllShifts(t: *Tree) void {
        t.root.applyAllShifts();
        t.shifts = null;
    }

    // Size returns the total number of intervals stored in the tree.
    fn size(t: *Tree) usize {
        return t.root.size();
    }
};

/// A key stores the start position of an interval, and a unique ID if you would
/// like to store multiple intervals starting from the same position. The key is
/// used for uniquely identifying a particular interval when searching or
/// removing from the tree.
pub const Key = struct {
    pos: usize,
    id: usize,

    pub fn init(pos: usize, id: usize) Key {
        return .{
            .pos = pos,
            .id = id,
        };
    }

    // compare orders keys by pos and then id.
    fn compare(k: Key, other: Key) isize {
        if (k.pos < other.pos) {
            return -1;
        } else if (k.pos > other.pos) {
            return 1;
        } else if (k.id < other.id) {
            return -1;
        } else if (k.id > other.id) {
            return 1;
        }
        return 0;
    }

    pub fn format(k: Key, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("Key{{{}:{}}}", .{ k.id, k.pos });
    }
};

pub const LazyInterval = struct {
    ins: std.ArrayListUnmanaged(Interval) = .{},
    n: ?*Node = null,

    pub fn deinit(i: *LazyInterval, allocator: mem.Allocator) void {
        i.ins.deinit(allocator);
    }

    pub fn high(i: LazyInterval) usize {
        var h: usize = 0;
        for (i.ins.items) |in| {
            const inh = in.high;
            if (inh > h) h = inh;
        }
        return h;
    }

    pub fn shift(i: *LazyInterval, amt: usize) void {
        for (0..i.ins.items.len) |j| {
            i.ins.items[j].high += amt;
            i.ins.items[j].low += amt;
        }
    }
};

pub const Node = struct {
    key: Key,
    max: usize,
    interval: LazyInterval,
    tstamp: u64, // timestamp to determine which shifts to apply
    tree: *Tree,

    // height counts nodes (not edges)
    height: isize,
    left: ?*Node,
    right: ?*Node,

    pub fn deinit(n_: ?*Node, allocator: mem.Allocator) void {
        const n = n_ orelse return;
        if (n.right) |r| deinit(r, allocator);
        if (n.left) |l| deinit(l, allocator);
        n.tree.deinit(allocator);
        n.interval.deinit(allocator);
        allocator.destroy(n);
        n.* = undefined;
    }

    pub fn format(n: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("node{{.max={} .left={?} .right={?}}}", .{ n.max, n.left, n.right });
    }

    // Adds a new node
    pub fn add(n_: ?*Node, allocator: mem.Allocator, tree: *Tree, key: Key, value: Interval) !struct { ?*Node, LazyInterval } {
        std.log.debug("lazylog.Node n=0x{x} add() key={} value={}", .{ @ptrToInt(n_), key, value });
        const n = n_ orelse {
            var nn = try allocator.create(Node);
            nn.* = Node{
                .tree = tree,
                .key = key,
                .max = value.high,
                .height = 1,
                .left = null,
                .right = null,
                .tstamp = tree.tstamp,
                .interval = undefined,
            };
            nn.interval = .{
                .ins = .{},
                .n = nn,
            };
            try nn.interval.ins.append(allocator, value);
            return .{ nn, nn.interval };
        };
        applyShifts(n);

        var loc: LazyInterval = undefined;
        const cmp = key.compare(n.key);
        std.log.debug("{}.compare({})={}", .{ key, n.key, cmp });
        if (cmp < 0) {
            const l_loc = try Node.add(n.left, allocator, tree, key, value);
            n.left = l_loc[0];
            loc = l_loc[1];
        } else if (cmp > 0) {
            const r_loc = try Node.add(n.right, allocator, tree, key, value);
            n.right = r_loc[0];
            loc = r_loc[1];
        } else {
            // if same key exists update value
            try n.interval.ins.append(allocator, value);
            n.tstamp = tree.tstamp;
            loc = n.interval;
        }
        return .{ Node.rebalanceTree(n), loc };
    }

    fn updateMax(mn: ?*Node) void {
        const n = mn orelse return;
        // if (mn) |n| {
        if (n.right) |right| {
            n.max = @max(n.max, right.max);
        }
        if (n.left) |left| {
            n.max = @max(n.max, left.max);
        }
        n.max = @max(n.max, n.interval.high());
        // }
        std.log.debug("updateMax() n.max={}", .{n.max});
    }

    // Removes a node
    fn remove(n_: ?*Node, allocator: mem.Allocator, key: Key) !?*Node {
        var n = n_ orelse return null;
        applyShifts(n);
        if (key.compare(n.key) < 0) {
            n.left = try remove(n.left, allocator, key);
        } else if (key.compare(n.key) > 0) {
            n.right = try remove(n.right, allocator, key);
        } else {
            if (n.left != null and n.right != null) {
                applyShifts(n.left);
                applyShifts(n.right);
                // node to delete found with both children;
                // replace values with smallest node of the right sub-tree
                const right_min_node = findSmallest(n.right).?;
                const rlen = right_min_node.interval.ins.items.len;
                n.key = right_min_node.key;
                if (n.interval.ins.items.len >= rlen) {
                    try n.interval.ins.ensureTotalCapacity(allocator, rlen);
                }
                n.interval.ins.items.len = rlen;
                @memcpy(n.interval.ins.items, right_min_node.interval.ins.items);

                n.interval.n = n;
                n.tstamp = right_min_node.tstamp;
                // delete smallest node that we replaced
                n.right = try remove(n.right, allocator, right_min_node.key);
            } else if (n.left != null) {
                applyShifts(n.left);
                // node only has left child
                n = n.left.?;
            } else if (n.right != null) {
                applyShifts(n.right);
                // node only has right child
                n = n.right.?;
            } else {
                // node has no children

                return null;
            }
        }
        return n.rebalanceTree();
    }

    // Searches for a node
    fn search(n_: ?*Node, allocator: mem.Allocator, key: Key) !?*Node {
        const n = n_ orelse return null;
        applyShifts(n);
        const cmp = key.compare(n.key);
        return if (cmp < 0)
            search(n.left, allocator, key)
        else if (cmp > 0)
            search(n.right, allocator, key)
        else
            n;
    }

    fn removeOverlaps(n_: ?*Node, allocator: mem.Allocator, low: usize, high: usize) !?*Node {
        var n = n_ orelse return n_;
        applyShifts(n);

        if (low > n.max) return n;
        {
            const tmp = n.left;
            n.left = try removeOverlaps(tmp, allocator, low, high);
        }

        var i: usize = 0;
        while (i < n.interval.ins.items.len) {
            if (n.interval.ins.items[i].overlaps(low, high)) {
                _ = n.interval.ins.swapRemove(i);
            } else {
                i += 1;
            }
        }

        if (n.interval.ins.items.len == 0) {
            const doright = high >= n.key.pos;
            const tmp = n;
            n = (try remove(tmp, allocator, n.key)) orelse return null;
            if (doright) {
                return removeOverlaps(n, allocator, low, high);
            }
            return n;
        }

        if (high < n.key.pos) {
            return n;
        }
        {
            const tmp = n.right;
            n.right = try removeOverlaps(tmp, allocator, low, high);
        }
        return n;
    }

    fn getHeight(n: ?*Node) isize {
        return (n orelse return 0).height;
    }

    fn getHeightLeft(n: ?*Node) isize {
        return ((n orelse return 0).left orelse return 0).height;
    }

    fn getHeightRight(n: ?*Node) isize {
        return ((n orelse return 0).right orelse return 0).height;
    }

    fn size(n_: ?*Node) usize {
        const n = n_ orelse return 0;
        return n.left.size() + n.right.size() + 1;
    }

    fn recalculateHeight(n: *Node) void {
        n.height = 1 + @max(getHeight(n.left), getHeight(n.right));
    }

    // Checks if node is balanced and rebalance
    fn rebalanceTree(n_: ?*Node) ?*Node {
        std.log.debug("rebalanceTree(0x{x})", .{@ptrToInt(n_)});
        const n = n_ orelse return n_;

        n.recalculateHeight();
        n.updateMax();

        // check balance factor and rotateLeft if right-heavy and rotateRight if left-heavy
        const balance_factor = getHeight(n.left) - getHeight(n.right);
        std.log.debug("rebalanceTree() balanceFactor={}", .{balance_factor});
        if (balance_factor <= -2) {
            // check if child is left-heavy and rotateRight first
            if (getHeightLeft(n.right) > getHeightRight(n.right)) {
                n.right = rotateRight(n.right);
            }
            return rotateLeft(n);
        } else if (balance_factor >= 2) {
            // check if child is right-heavy and rotateLeft first
            if (getHeightRight(n.left) > getHeightLeft(n.left)) {
                n.left = rotateLeft(n.left);
            }

            return rotateRight(n);
        }
        return n;
    }

    // Rotate nodes left to balance node
    fn rotateLeft(n_: ?*Node) ?*Node {
        const n = n_ orelse return n_;
        applyShifts(n);
        if (n.right != null) {
            applyShifts(n.right);
        }

        const root = n.right;
        n.right = if (root) |r| r.left else null;
        if (root) |r| r.left = n;

        n.recalculateHeight();
        n.updateMax();
        if (root) |r| r.recalculateHeight();
        updateMax(root);
        return root;
    }

    // Rotate nodes right to balance node
    fn rotateRight(n_: ?*Node) ?*Node {
        const n = n_ orelse return n_;
        applyShifts(n);
        if (n.left) |left| {
            applyShifts(left);
        }

        const root = n.left;
        n.left = if (root) |r| r.right else null;
        if (root) |r| r.right = n;

        recalculateHeight(n);
        updateMax(n);
        if (root) |r| r.recalculateHeight();
        updateMax(root);
        return root;
    }

    // Finds the smallest child (based on the key) for the current node
    fn findSmallest(n_: ?*Node) ?*Node {
        const n = n_.?;
        if (n.left != null) {
            applyShifts(n.left);
            return findSmallest(n.left);
        } else {
            return n;
        }
    }

    fn applyShift(n: *Node, s: Shift) void {
        if (n.tstamp >= s.tstamp) {
            // this shift is outdated and we have already applied it
            return;
        }

        n.tstamp = s.tstamp;
        if (n.max < s.idx) return;

        const amt = @intCast(usize, s.amt);
        n.max += amt;
        if (n.key.pos >= s.idx) {
            n.key.pos += amt;
            n.interval.shift(amt);
        }
        n.updateMax();
    }

    pub fn applyShifts(n_: ?*Node) void {
        const n = n_ orelse return;
        // optimization: first check if we are completely up-to-date and if so
        // there is nothing to do.
        if (n.tree.shifts.items.len == 0 or
            n.tstamp >= n.tree.shifts.items[n.tree.shifts.items.len - 1].tstamp)
        {
            return;
        }
        // optimization: search backwards to find the starting point. Alternatively
        // we could binary search? not sure which is faster.
        const j = blk: {
            var j = @bitCast(isize, n.tree.shifts.items.len) - 1;
            while (j > 0) : (j -= 1) {
                if (n.tstamp >= n.tree.shifts.items[@bitCast(usize, j)].tstamp) {
                    j = j + 1;
                    break;
                }
            }
            break :blk @bitCast(usize, j);
        };
        for (0..n.tree.shifts.items[j..].len) |i|
            applyShift(n, n.tree.shifts.items[j + i]);
    }

    fn applyAllShifts(n_: ?*Node, allocator: mem.Allocator) void {
        _ = allocator;
        const n = n_ orelse return;

        n.left.applyAllShifts();
        n.right.applyAllShifts();
        applyShifts(n);
    }

    fn eachNode(n_: ?*Node, allocator: mem.Allocator, f: fn (*Node) void) void {
        _ = allocator;
        const n = n_ orelse return;

        n.left.eachNode(f);
        applyShifts(n);
        f(n);
        n.right.eachNode(f);
    }
};
