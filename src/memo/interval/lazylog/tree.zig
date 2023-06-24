const std = @import("std");
const mem = std.mem;
const lazytree = @import("../lazy/tree.zig");
const Value = lazytree.Value;

pub const Interval = struct {
    low: isize,
    high: isize,
    value: Value,

    pub const List = std.ArrayListUnmanaged(Interval);

    pub fn len(i: Interval) isize {
        return i.high - i.low;
    }

    // returns true if i overlaps with the interval [low:high)
    pub fn overlaps(i: Interval, low: isize, high: isize) bool {
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
    idx: isize,
    amt: isize,
    tstamp: u64,
};

/// ShiftThreshold is the number of shifts to accumulate before applying all
/// shifts.
const ShiftThreshold = -1;

pub const Tree = struct {
    root: ?*Node = null,
    shifts: std.ArrayListUnmanaged(Shift) = .{},
    tstamp: u64 = 0, // most recent timestamp

    pub fn deinit(t: *Tree, allocator: mem.Allocator) void {
        Node.deinit(t.root, allocator);
        t.shifts.deinit(allocator);
    }

    pub fn format(t: Tree, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Tree{{ .root={*} .shifts={{.len={} }}}}", .{ t.root, t.shifts.items.len });
        // try writer.print("Tree{{ .root={*} .shifts={{", .{t.root});
        // for (t.shifts.items, 0..) |sh, i| {
        //     if (i != 0) _ = try writer.write(", ");
        //     try writer.print("{}", .{sh});
        // }
        // _ = try writer.write("} }");
        try t.dump(fmt, options, writer);
    }
    pub fn dump(t: Tree, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        const E = error{};
        const Ctx = @TypeOf(writer);
        try writer.print("root={?}", .{t.root});
        try Node.eachNode(t.root, Ctx, E, writer, struct {
            fn func(mn: ?*Node, ctx: Ctx) E!void {
                _ = ctx;
                _ = mn;
                unreachable;
            }
        }.func);
    }

    // Adds the given interval to the tree. An id should also be given to the
    // interval to uniquely identify it if any other intervals begin at the same
    // location.
    pub fn add(t: *Tree, allocator: mem.Allocator, id: usize, low: isize, high: isize, value: Value) !Value {
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
    pub fn findLargest(t: *Tree, id: usize, pos: isize) ?Value {
        const n = Node.search(t.root, Key{
            .pos = pos,
            .id = id,
        }) orelse return null;

        if (n.interval.ins.items.len == 0)
            return null;

        var max: usize = 0;
        for (n.interval.ins.items[1..], 1..) |in, i| {
            if (in.len() > n.interval.ins.items[max].len()) {
                max = i;
            }
        }

        return n.interval.ins.items[max].value;
    }

    pub fn removeAndShift(t: *Tree, allocator: mem.Allocator, low: isize, high: isize, amt: isize) !void {
        std.debug.assert(low < high);
        t.root = try Node.removeOverlaps(t.root, allocator, low, high);
        if (amt != 0) {
            try t.shift(allocator, low, amt);
        }
    }

    // Shift all intervals in the tree after idx by amt. The shift idx should not
    // lie inside an interval. This could conceivably be implemented, but is not
    // currently. If a negative shift is performed, ensure that there is space for
    // all intervals to be shifted left without overlapping with another interval.
    fn shift(t: *Tree, allocator: mem.Allocator, idx: isize, amt: isize) !void {
        if (amt == 0) return;

        t.tstamp += 1;
        try t.shifts.append(allocator, .{
            .idx = idx,
            .amt = amt,
            .tstamp = t.tstamp,
        });
        if (ShiftThreshold != -1 and t.shifts.items.len >= ShiftThreshold) {
            t.applyAllShifts();
        }
    }

    fn applyAllShifts(t: *Tree) void {
        std.log.debug("t.applyAllShifts() {any}", .{t.shifts.items});
        Node.applyAllShifts(t.root);
        t.shifts.clearRetainingCapacity();
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
    pos: isize,
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
    ins: Interval.List = .{},
    n: ?*Node,

    pub fn deinit(i: *LazyInterval, allocator: mem.Allocator) void {
        i.ins.deinit(allocator);
    }

    pub fn high(i: LazyInterval) isize {
        var h: isize = 0;
        for (i.ins.items) |in| {
            const inh = in.high;
            if (inh > h) h = inh;
        }
        return h;
    }

    pub fn shift(i: *LazyInterval, amt: isize) void {
        // std.log.debug("LazyInterval.shift() amt={}", .{amt});
        for (0..i.ins.items.len) |j| {
            // std.log.debug("  i.ins.items[{}]={}", .{ j, i.ins.items[j] });
            i.ins.items[j].high += amt;
            i.ins.items[j].low += amt;
        }
        std.log.debug("LazyInterval.shift() ins={any}", .{i.ins.items});
    }
};

pub const Node = struct {
    key: Key,
    max: isize,
    interval: *LazyInterval,
    tstamp: u64, // timestamp to determine which shifts to apply
    tree: *Tree,

    // height counts nodes (not edges)
    height: isize,
    left: ?*Node,
    right: ?*Node,

    pub fn deinit(mn: ?*Node, allocator: mem.Allocator) void {
        const n = mn orelse return;
        if (n.right) |r| deinit(r, allocator);
        if (n.left) |l| deinit(l, allocator);
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
    pub fn add(mn: ?*Node, allocator: mem.Allocator, tree: *Tree, key: Key, interval: Interval) !struct { ?*Node, *LazyInterval } {
        std.log.debug("lazylog.Node n=0x{x} add() key={} value={}", .{ @intFromPtr(mn), key, interval });
        const n = mn orelse {
            const nn = try allocator.create(Node);
            var ins = try Interval.List.initCapacity(allocator, 1);
            ins.appendAssumeCapacity(interval);
            const lazy_interval = try allocator.create(LazyInterval);
            lazy_interval.* = .{ .ins = ins, .n = nn };
            nn.* = Node{
                .tree = tree,
                .key = key,
                .max = interval.high,
                .height = 1,
                .left = null,
                .right = null,
                .tstamp = tree.tstamp,
                .interval = lazy_interval,
            };
            return .{ nn, nn.interval };
        };
        applyShifts(n);

        const cmp = key.compare(n.key);
        std.log.debug("{}.compare({})={}", .{ key, n.key, cmp });
        const loc = if (cmp < 0) loc: {
            const l_loc = try Node.add(n.left, allocator, tree, key, interval);
            n.left = l_loc[0];
            break :loc l_loc[1];
        } else if (cmp > 0) loc: {
            const r_loc = try Node.add(n.right, allocator, tree, key, interval);
            n.right = r_loc[0];
            break :loc r_loc[1];
        } else loc: {
            // if same key exists update value
            try n.interval.ins.append(allocator, interval);
            n.tstamp = tree.tstamp;
            break :loc n.interval;
        };
        return .{ n.rebalanceTree(), loc };
    }

    fn updateMax(mn: ?*Node) void {
        const n = mn orelse return;
        if (n.right) |right| {
            n.max = @max(n.max, right.max);
        }
        if (n.left) |left| {
            n.max = @max(n.max, left.max);
        }
        n.max = @max(n.max, n.interval.high());
        std.log.debug("updateMax() n.max={}", .{n.max});
    }

    // Removes a node
    fn remove(mn: ?*Node, allocator: mem.Allocator, key: Key) !?*Node {
        var n = mn orelse return null;
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
                try n.interval.ins.ensureTotalCapacity(allocator, rlen);
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
                // n.deinit(allocator);
                return null;
            }
        }
        return n.rebalanceTree();
    }

    // Searches for a node
    fn search(mn: ?*Node, key: Key) ?*Node {
        const n = mn orelse return null;
        applyShifts(n);
        const cmp = key.compare(n.key);
        return if (cmp < 0)
            search(n.left, key)
        else if (cmp > 0)
            search(n.right, key)
        else
            n;
    }

    fn removeOverlaps(mn: ?*Node, allocator: mem.Allocator, low: isize, high: isize) !?*Node {
        var n = mn orelse return mn;
        n.applyShifts();

        if (low > n.max) return n;

        n.left = try removeOverlaps(n.left, allocator, low, high);

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
            n = (try n.remove(allocator, n.key)) orelse return null;
            if (doright) {
                return n.removeOverlaps(allocator, low, high);
            }
            return n;
        }

        if (high < n.key.pos) return n;

        n.right = try removeOverlaps(n.right, allocator, low, high);

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

    pub fn size(mn: ?*Node) usize {
        const n = mn orelse return 0;
        return size(n.left) + size(n.right) + n.interval.ins.items.len;
    }

    fn recalculateHeight(n: *Node) void {
        n.height = 1 + @max(getHeight(n.left), getHeight(n.right));
    }

    // Checks if node is balanced and rebalance
    fn rebalanceTree(n: *Node) ?*Node {
        std.log.debug("rebalanceTree(0x{x})", .{@intFromPtr(n)});

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
            return n.rotateLeft();
        } else if (balance_factor >= 2) {
            // check if child is right-heavy and rotateLeft first
            if (getHeightRight(n.left) > getHeightLeft(n.left)) {
                n.left = rotateLeft(n.left);
            }

            return n.rotateRight();
        }
        return n;
    }

    // Rotate nodes left to balance node
    fn rotateLeft(mn: ?*Node) ?*Node {
        const n = mn orelse return mn;
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
    fn rotateRight(mn: ?*Node) ?*Node {
        const n = mn orelse return mn;
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
    fn findSmallest(mn: ?*Node) ?*Node {
        const n = mn.?;
        if (n.left != null) {
            applyShifts(n.left);
            return findSmallest(n.left);
        } else {
            return n;
        }
    }

    fn applyShift(n: *Node, s: Shift) void {
        // std.log.info("n.applyShift() s={} n.tstamp={} n.max={}", .{ s, n.tstamp, n.max });
        if (n.tstamp >= s.tstamp) {
            // this shift is outdated and we have already applied it
            return;
        }

        n.tstamp = s.tstamp;
        if (n.max < s.idx) return;

        const amt = s.amt;
        n.max += amt;
        if (n.key.pos >= s.idx) {
            n.key.pos += amt;
            n.interval.shift(amt);
        }
        std.log.debug("n.key.pos={}", .{n.key.pos});
        n.updateMax();
    }

    pub fn applyShifts(mn: ?*Node) void {
        const n = mn orelse return;
        std.log.debug("Node.applyShifts() n.tree.shifts.len={} n.tstamp={}", .{ n.tree.shifts.items.len, n.tstamp });
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
            n.applyShift(n.tree.shifts.items[j + i]);
    }

    fn applyAllShifts(mn: ?*Node) void {
        const n = mn orelse return;
        // std.log.debug("Node.applyAllShifts()", .{});
        Node.applyAllShifts(n.left);
        Node.applyAllShifts(n.right);
        n.applyShifts();
    }

    pub fn eachNode(
        mn: ?*Node,
        comptime Ctx: type,
        comptime Err: type,
        ctx: Ctx,
        comptime f: fn (?*Node, Ctx) Err!void,
    ) Err!void {
        const n = mn orelse return;
        try eachNode(n.left, Ctx, Err, ctx, f);
        n.applyShifts();
        try f(n, ctx);
        try eachNode(n.right, Ctx, Err, ctx, f);
    }
};
