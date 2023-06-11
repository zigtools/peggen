const std = @import("std");
const mem = std.mem;
const memo = @import("memo.zig");
const Interval = memo.Interval;
const IValues = memo.IValues;
const LazyInterval = memo.LazyInterval;

/// A shift of intervals in the tree. The shift starts at idx and moves
/// intervals after idx by amt. Shifts are lazily applied in the tree to avoid
/// linear time costs.
pub const Shift = struct {
    idx: usize,
    amt: usize,
    tstamp: u64,
};

/// ShiftThreshold is the number of shifts to accumulate before applying all
/// shifts.
const ShiftThreshold = -1;

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
};

pub const Tree = struct {
    root: ?*LazyNode,
    shifts: std.ArrayListUnmanaged(Shift) = .{},
    tstamp: u64 = 0, // most recent timestamp

    pub inline fn init(allocator: mem.Allocator) !Tree {
        const root = try allocator.create(LazyNode);
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
        LazyNode.deinit(t.root, allocator);
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
    pub fn add(t: *Tree, allocator: mem.Allocator, id: usize, low: usize, high: usize, value: usize) !usize {
        std.debug.assert(low < high);
        const r_loc = try LazyNode.add(
            t.root,
            allocator,
            t,
            Key{ .pos = low, .id = id },
            Interval{ .low = low, .high = high, .value = value },
        );
        t.root = r_loc[0];
        return r_loc[1];
    }

    // Search for the interval starting at pos with the given id. Returns null if no
    // such interval exists.
    pub fn findLargest(t: *Tree, allocator: mem.Allocator, id: usize, pos: usize) !?*Interval {
        const mn = try LazyNode.search(t.root, allocator, Key{
            .pos = pos,
            .id = id,
        });

        const n = mn orelse return null;
        if (n.interval.ins.items.len == 0) {
            return null;
        }

        var max: usize = 0;
        for (n.interval.ins.items[1..], 0..) |in, i| {
            if (in.len() > n.interval.ins.items[max].len()) {
                max = i + 1;
            }
        }

        return &n.interval.ins.items[max];
    }

    pub fn removeAndShift(t: *Tree, allocator: mem.Allocator, low: usize, high: usize, amt: usize) !void {
        std.debug.assert(low < high);
        const tmp = t.root;
        t.root = LazyNode.removeOverlaps(tmp, low, high);
        if (amt != 0) {
            try t.shift(allocator, low, amt);
        }
    }

    // Shift all intervals in the tree after idx by amt. The shift idx should not
    // lie inside an interval. This could conceivably be implemented, but is not
    // currently. If a negative shift is performed, ensure that there is space for
    // all intervals to be shifted left without overlapping with another interval.
    fn shift(t: *Tree, allocator: mem.Allocator, idx: usize, amt: usize) !void {
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

pub const Node = struct {
    key: Key,
    max: usize,
    iv: IValues,
    shifts: std.ArrayListUnmanaged(Shift) = .{},

    // height counts nodes (not edges)
    height: usize,
    left: ?*Node,
    right: ?*Node,
    parent: ?*Node,

    pub fn deinit(n_: ?*Node, allocator: mem.Allocator) void {
        const n = n_ orelse return;
        if (n.right) |r| deinit(r, allocator);
        if (n.left) |l| deinit(l, allocator);
        n.shifts.deinit(allocator);
        n.iv.deinit(allocator);
        allocator.destroy(n);
        n.* = undefined;
    }

    pub fn format(n: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("node{{.key={{{},{}}} .left={?} .right={?}}}", .{ n.key.id, n.key.pos, n.left, n.right });
    }

    fn addShift(n: ?*Node, allocator: mem.Allocator, sh: Shift) !void {
        try (n orelse return).shifts.append(allocator, sh);
    }

    fn applyShifts(n_: ?*Node, allocator: mem.Allocator) !void {
        var n = n_ orelse return;
        for (n.shifts.items) |sh| {
            if (n.max >= sh.idx) {
                if (n.key.pos >= sh.idx) {
                    n.key.pos += sh.amt;
                    for (n.iv.ivs.items, 0..) |iv, i| {
                        n.iv.ivs.items[i].interval = iv.interval.shift(sh.amt);
                    }
                }
                n.max += sh.amt;
                // n.updateMax()
            }

            try addShift(n.left, allocator, sh);
            try addShift(n.right, allocator, sh);
        }
        n.shifts.clearRetainingCapacity();
    }

    pub fn add(n_: ?*Node, allocator: mem.Allocator, key: Key, high: usize, value: usize, parent: ?*Node) !struct { ?*Node, IValues } {
        var n = n_ orelse {
            var n = try allocator.create(Node);
            n.* = Node{
                .key = key,
                .max = high,
                .iv = IValues{
                    .ivs = .{},
                    .node = n,
                },
                .height = 1,
                .left = null,
                .right = null,
                .parent = parent,
            };
            try n.iv.ivs.append(allocator, .{
                .interval = Interval.init(key.pos, high),
                .value = value,
            });
            return .{ n, n.iv };
        };
        try n.applyShifts(allocator);

        var iv: IValues = undefined;
        if (key.compare(n.key) < 0) {
            const l_iv = try Node.add(n.left, allocator, key, high, value, n);
            n.left = l_iv[0];
            iv = l_iv[1];
        } else if (key.compare(n.key) > 0) {
            const r_iv = try Node.add(n.right, allocator, key, high, value, n);
            n.right = r_iv[0];
            iv = r_iv[1];
        } else {
            // if same key exists update value
            try n.iv.ivs.append(allocator, memo.IValue{
                .interval = Interval.init(key.pos, high),
                .value = value,
            });
            iv = n.iv;
        }
        return .{ try rebalanceTree(n, allocator, parent), iv };
    }

    fn calcMax(n: *Node) usize {
        var max: usize = 0;
        for (n.iv.ivs.items) |iv| {
            if (iv.interval.High > max) {
                max = iv.interval.High;
            }
        }
        return max;
    }

    fn updateMax(n_: ?*Node) void {
        const n = n_ orelse return;
        if (n.right) |right| {
            n.max = @max(n.max, right.max);
        }
        if (n.left) |left| {
            n.max = @max(n.max, left.max);
        }
        n.max = @max(n.max, n.calcMax());
    }

    fn remove(n_: ?*Node, allocator: mem.Allocator, key: Key, parent: ?*Node) !?*Node {
        var n = n_ orelse return null;
        try applyShifts(n, allocator);
        if (key.compare(n.key) < 0) {
            n.left = try remove(n.left, allocator, key, n);
        } else if (key.compare(n.key) > 0) {
            n.right = try remove(n.right, allocator, key, n);
        } else {
            if (n.left != null and n.right != null) {
                try applyShifts(n.left, allocator);
                try applyShifts(n.right, allocator);
                // node to delete found with both children;
                // replace values with smallest node of the right sub-tree
                const rightMinNode = try n.right.?.findSmallest(allocator);
                n.key = rightMinNode.key;
                n.iv = rightMinNode.iv;
                n.iv.node = n;
                n.shifts = rightMinNode.shifts;
                // delete smallest node that we replaced
                n.right = try remove(n.right, allocator, rightMinNode.key, n);
            } else if (n.left != null) {
                try applyShifts(n.left, allocator);
                // node only has left child
                deinit(n.right, allocator);
                n = n.left.?;
            } else if (n.right != null) {
                try applyShifts(n.right, allocator);
                // node only has right child
                deinit(n.left, allocator);
                n = n.right.?;
            } else {
                // node has no children
                deinit(n, allocator);
                return null;
            }
        }
        n.parent = parent;
        return rebalanceTree(n, allocator, parent);
    }

    fn search(n_: ?*Node, allocator: mem.Allocator, key: Key) !?*Node {
        const n = n_ orelse return null;
        try applyShifts(n, allocator);
        if (key.compare(n.key) < 0) {
            return search(n.left, allocator, key);
        } else if (key.compare(n.key) > 0) {
            return search(n.right, allocator, key);
        } else {
            return n;
        }
    }

    // fn overlaps(n: *Node, low: usize, high: usize, result: []map.Value) []map.Value {
    //     if (n == null) {
    //         return result;
    //     }

    //     n.applyShifts();

    //     if (low >= n.max) {
    //         return result;
    //     }

    //     result = n.left.overlaps(low, high, result);

    //     for (n.iv.ivs.items) |iv| {
    //         if (iv.interval.Overlaps(Interval{ low, high })) {
    //             result.append(iv.value);
    //         }
    //     }

    //     if (high <= n.key.pos) {
    //         return result;
    //     }

    //     result = n.right.overlaps(low, high, result);
    //     return result;
    // }

    fn removeOverlaps(n_: ?*Node, allocator: mem.Allocator, low: usize, high: usize, parent: ?*Node) !?*Node {
        var n = n_ orelse return n_;
        // std.debug.print("removeOverlaps n={?}\n", .{n});
        try n.applyShifts(allocator);

        if (low >= n.max) return n;

        n.left = try removeOverlaps(n.left, allocator, low, high, n);

        var i: usize = 0;
        while (i < n.iv.ivs.items.len) {
            if (n.iv.ivs.items[i].interval.Overlaps(Interval.init(low, high))) {
                _ = n.iv.ivs.swapRemove(i);
            } else {
                i += 1;
            }
        }

        if (n.iv.ivs.items.len == 0) {
            const doright = high > n.key.pos;
            const nn = try remove(n, allocator, n.key, parent);
            if (doright) {
                return removeOverlaps(nn, allocator, low, high, parent);
            }
            return nn;
        }

        if (high <= n.key.pos) return n;

        // std.debug.print("n.right={?}\n", .{n.right});
        n.right = try removeOverlaps(n.right, allocator, low, high, n);
        return n;
    }

    fn getHeight(mn: ?*Node) usize {
        return if (mn) |n| n.height else 0;
    }

    fn getHeightLeft(mn: ?*Node) usize {
        const n = mn orelse return 0;
        const l = n.left orelse return 0;
        return l.height;
    }

    fn getHeightRight(mn: ?*Node) usize {
        const n = mn orelse return 0;
        const l = n.right orelse return 0;
        return l.height;
    }

    fn getSize(n: *Node) usize {
        if (n == null) {
            return 0;
        }
        return n.left.getSize() + n.right.getSize() + 1;
    }

    fn updateHeightAndMax(n: *Node) void {
        n.height = 1 + @max(getHeight(n.left), getHeight(n.right));
        n.updateMax();
    }

    // Checks if node is balanced and rebalance
    fn rebalanceTree(n_: ?*Node, allocator: mem.Allocator, parent: ?*Node) !?*Node {
        var n = n_ orelse return n_;
        n.updateHeightAndMax();

        // check balance factor and rotateLeft if right-heavy and rotateRight if left-heavy
        const balanceFactor = getHeight(n.left) - getHeight(n.right);
        if (balanceFactor == -2) {
            // check if child is left-heavy and rotateRight first
            if (getHeightLeft(n.right) > getHeightRight(n.right)) {
                n.right = try rotateRight(n.right, allocator, n);
            }
            return try rotateLeft(n, allocator, parent);
        } else if (balanceFactor == 2) {
            // check if child is right-heavy and rotateLeft first
            if (getHeightRight(n.left) > getHeightLeft(n.left)) {
                n.left = try rotateLeft(n.left, allocator, n);
            }
            return try rotateRight(n, allocator, parent);
        }
        return n;
    }

    // Rotate nodes left to balance node
    fn rotateLeft(n_: ?*Node, allocator: mem.Allocator, newParent: ?*Node) !?*Node {
        const n = n_ orelse return n_;
        try applyShifts(n, allocator);
        if (n.right) |right| try applyShifts(right, allocator);

        const newRoot = n.right orelse return n.right;
        n.right = newRoot.left;
        if (newRoot.left) |left| left.parent = n;

        newRoot.left = n;
        n.parent = newRoot;
        newRoot.parent = newParent;

        n.updateHeightAndMax();
        newRoot.updateHeightAndMax();
        return newRoot;
    }

    // Rotate nodes right to balance node
    fn rotateRight(n_: ?*Node, allocator: mem.Allocator, newParent: ?*Node) !?*Node {
        const n = n_ orelse return n_;
        try applyShifts(n, allocator);
        if (n.left) |left| {
            try left.applyShifts(allocator);
        }

        const newRoot = n.left orelse return n.left;
        n.left = newRoot.right;
        if (newRoot.right) |right| {
            right.parent = n;
        }
        newRoot.right = n;
        n.parent = newRoot;
        newRoot.parent = newParent;

        n.updateHeightAndMax();
        newRoot.updateHeightAndMax();
        return newRoot;
    }

    // Finds the smallest child (based on the key) for the current node
    fn findSmallest(n: *Node, allocator: mem.Allocator) !*Node {
        if (n.left != null) {
            try applyShifts(n.left, allocator);
            return try n.left.?.findSmallest(allocator);
        } else {
            return n;
        }
    }

    pub fn applyAllShifts(n_: ?*Node, allocator: mem.Allocator) !void {
        const n = n_ orelse return;
        if (n.parent) |parent| {
            if (parent != n) try applyAllShifts(n.parent, allocator);
        }
        try applyShifts(n, allocator);
    }
};

pub const LazyNode = struct {
    key: Key,
    max: usize,
    interval: memo.LazyInterval,
    tstamp: u64, // timestamp to determine which shifts to apply
    tree: *Tree,

    // height counts nodes (not edges)
    height: usize,
    left: ?*LazyNode,
    right: ?*LazyNode,

    pub fn deinit(n_: ?*LazyNode, allocator: mem.Allocator) void {
        const n = n_ orelse return;
        if (n.right) |r| deinit(r, allocator);
        if (n.left) |l| deinit(l, allocator);
        // n.tree.deinit(allocator);
        n.interval.deinit(allocator);
        allocator.destroy(n);
        n.* = undefined;
    }

    pub fn format(n: LazyNode, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("node{{.max={} .left={?} .right={?}}}", .{ n.max, n.left, n.right });
    }

    // Adds a new node
    pub fn add(n_: ?*LazyNode, allocator: mem.Allocator, tree: *Tree, key: Key, value: LazyInterval.Data) !struct { ?*LazyNode, LazyInterval } {
        const n = n_ orelse {
            var nn = try allocator.create(LazyNode);
            nn.* = LazyNode{
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
        if (key.compare(n.key) < 0) {
            const l_loc = try LazyNode.add(n.left, allocator, tree, key, value);
            n.left = l_loc[0];
            loc = l_loc[1];
        } else if (key.compare(n.key) > 0) {
            const r_loc = try LazyNode.add(n.right, allocator, tree, key, value);
            n.right = r_loc[0];
            loc = r_loc[1];
        } else {
            // if same key exists update value
            try n.interval.ins.append(allocator, value);
            n.tstamp = tree.tstamp;
            loc = n.interval;
        }
        return .{ LazyNode.rebalanceTree(n), loc };
    }

    fn updateMax(mn: ?*LazyNode) void {
        if (mn) |n| {
            if (n.right) |right| {
                n.max = @max(n.max, right.max);
            }
            if (n.left) |left| {
                n.max = @max(n.max, left.max);
            }
            n.max = @max(n.max, n.interval.high());
        }
    }

    // Removes a node
    fn remove(n_: ?*LazyNode, key: Key) ?*LazyNode {
        var n = n_ orelse return null;
        applyShifts(n);
        if (key.compare(n.key) < 0) {
            n.left = remove(n.left, key);
        } else if (key.compare(n.key) > 0) {
            n.right = remove(n.right, key);
        } else {
            if (n.left != null and n.right != null) {
                applyShifts(n.left);
                applyShifts(n.right);
                // node to delete found with both children;
                // replace values with smallest node of the right sub-tree
                const rightMinNode = findSmallest(n.right).?;

                n.key = rightMinNode.key;
                // try n.interval.ins.ensureTotalCapacity(allocator, rightMinNode.interval.ins.items.len);
                std.debug.assert(n.interval.ins.items.len >= rightMinNode.interval.ins.items.len);
                n.interval.ins.items.len = rightMinNode.interval.ins.items.len;
                @memcpy(n.interval.ins.items, rightMinNode.interval.ins.items);
                n.interval.n = n;
                n.tstamp = rightMinNode.tstamp;
                // delete smallest node that we replaced
                n.right = remove(n.right, rightMinNode.key);
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
    fn search(n_: ?*LazyNode, allocator: mem.Allocator, key: Key) !?*LazyNode {
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

    fn removeOverlaps(n_: ?*LazyNode, low: usize, high: usize) ?*LazyNode {
        var n = n_ orelse return n_;
        applyShifts(n);

        if (low > n.max) return n;
        {
            const tmp = n.left;
            n.left = removeOverlaps(tmp, low, high);
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
            n = remove(tmp, n.key) orelse return null;
            if (doright) {
                return removeOverlaps(n, low, high);
            }
            return n;
        }

        if (high < n.key.pos) {
            return n;
        }
        {
            const tmp = n.right;
            n.right = removeOverlaps(tmp, low, high);
        }
        return n;
    }

    // fn allvals(n_: ?*Node, vals: []intval.Value) []intval.Value {
    //     const n = n_ orelse return vals;

    //     vals = n.left.allvals(vals);

    //     for (n.interval.ins) |in|
    //         try vals.append(in.value);

    //     vals = n.right.allvals(vals);

    //     return vals;
    // }

    fn getHeight(n: ?*LazyNode) usize {
        return (n orelse return 0).height;
    }

    fn getHeightLeft(n: ?*LazyNode) usize {
        return ((n orelse return 0).left orelse return 0).height;
    }

    fn getHeightRight(n: ?*LazyNode) usize {
        return ((n orelse return 0).right orelse return 0).height;
    }

    fn size(n_: ?*LazyNode) usize {
        const n = n_ orelse return 0;
        return n.left.size() + n.right.size() + 1;
    }

    fn recalculateHeight(n: *LazyNode) void {
        n.height = 1 + @max(getHeight(n.left), getHeight(n.right));
    }

    // Checks if node is balanced and rebalance
    fn rebalanceTree(n_: ?*LazyNode) ?*LazyNode {
        const n = n_ orelse return n_;

        n.recalculateHeight();
        n.updateMax();

        // check balance factor and rotateLeft if right-heavy and rotateRight if left-heavy
        const balanceFactor = getHeight(n.left) - getHeight(n.right);
        if (balanceFactor <= -2) {
            // check if child is left-heavy and rotateRight first
            if (getHeightLeft(n.right) > getHeightRight(n.right)) {
                n.right = rotateRight(n.right);
            }
            return rotateLeft(n);
        } else if (balanceFactor >= 2) {
            // check if child is right-heavy and rotateLeft first
            if (getHeightRight(n.left) > getHeightLeft(n.left)) {
                n.left = rotateLeft(n.left);
            }

            return rotateRight(n);
        }
        return n;
    }

    // Rotate nodes left to balance node
    fn rotateLeft(n_: ?*LazyNode) ?*LazyNode {
        const n = n_ orelse return n_;
        applyShifts(n);
        if (n.right != null) {
            applyShifts(n.right);
        }

        const newRoot = n.right orelse return n.right;
        n.right = newRoot;
        newRoot.left = n;

        n.recalculateHeight();
        n.updateMax();
        newRoot.recalculateHeight();
        newRoot.updateMax();
        return newRoot;
    }

    // Rotate nodes right to balance node
    fn rotateRight(n_: ?*LazyNode) ?*LazyNode {
        const n = n_ orelse return n_;
        applyShifts(n);
        if (n.left) |left| {
            applyShifts(left);
        }

        const newRoot = n.left orelse return n.left;
        n.left = newRoot.right;
        newRoot.right = n;

        n.recalculateHeight();
        n.updateMax();
        newRoot.recalculateHeight();
        newRoot.updateMax();
        return newRoot;
    }

    // Finds the smallest child (based on the key) for the current node
    fn findSmallest(n_: ?*LazyNode) ?*LazyNode {
        const n = n_.?;
        if (n.left != null) {
            applyShifts(n.left);
            return findSmallest(n.left);
        } else {
            return n;
        }
    }

    fn applyShift(n: *LazyNode, s: Shift) void {
        if (n.tstamp >= s.tstamp) {
            // this shift is outdated and we have already applied it
            return;
        }

        n.tstamp = s.tstamp;
        if (n.max < s.idx) return;

        n.max += s.amt;
        if (n.key.pos >= s.idx) {
            n.key.pos += s.amt;
            n.interval.shift(s.amt);
        }
        n.updateMax();
    }

    pub fn applyShifts(n_: ?*LazyNode) void {
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
            var j = @bitCast(isize, n.tree.shifts.items.len - 1);
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

    fn applyAllShifts(n_: ?*LazyNode, allocator: mem.Allocator) void {
        _ = allocator;
        const n = n_ orelse return;

        n.left.applyAllShifts();
        n.right.applyAllShifts();
        applyShifts(n);
    }

    fn eachNode(n_: ?*LazyNode, allocator: mem.Allocator, f: fn (*LazyNode) void) void {
        _ = allocator;
        const n = n_ orelse return;

        n.left.eachNode(f);
        applyShifts(n);
        f(n);
        n.right.eachNode(f);
    }
};
