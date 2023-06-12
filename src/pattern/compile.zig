const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const pattern = @import("../pattern.zig");
const Pattern = pattern.Pattern;
const Ptr = Pattern.Ptr;
const Charset = pattern.Charset;
const isa = @import("../isa.zig");
const Insn = isa.Insn;
const Program = isa.Program;
const optimize = @import("../optimize.zig");

pub const Error = error{ OutOfMemory, NotFound, InvalidLiteral, InvalidNot };

pub fn compile(p: Pattern, allocator: mem.Allocator) Error!Program {
    std.log.debug("compile {s}", .{@tagName(p)});
    switch (p) {
        .grammar => |n| {
            while (try inline_(p, allocator)) {}
            const Used = std.StringHashMap(void);
            var used = Used.init(allocator);
            defer used.deinit();
            var iter = n.rules.iterator();
            while (iter.next()) |e| {
                const v = e.value_ptr;
                const W = struct {
                    used: *Used,
                    pub fn walk(w: @This(), pat: Ptr) !void {
                        // std.log.debug("walk {s} {} {}", .{ @tagName(pat.*), @enumToInt(pat.*), @enumToInt(Pattern.Tag.memo) });
                        if (pat.* == .non_term) {
                            if (pat.non_term.inlined == null)
                                try w.used.put(pat.non_term.name, {});
                        }
                    }
                };
                var w = W{ .used = &used };
                try v.walk(true, W, w);
            }

            if (used.count() == 0)
                return n.rules.get(n.start).?.compile(allocator);

            var code = Program{};
            const lend = isa.Label.init();
            try code.appendSlice(allocator, &.{
                Insn.init(.open_call, n.start),
                Insn.init(.jump, lend),
            });

            var labels = std.StringHashMap(isa.Label).init(allocator);
            defer labels.deinit();
            iter.index = 0;
            while (iter.next()) |e| {
                const k = e.key_ptr.*;
                const v = e.value_ptr.*;
                // std.log.debug("grammar compile() key={s}", .{k});
                if (!mem.eql(u8, k, n.start) and !used.contains(k))
                    continue;
                const label = isa.Label.init();
                try labels.put(k, label);
                var f = try v.compile(allocator);
                defer f.deinit(allocator);
                try code.append(allocator, label.toInsn());
                try code.appendSlice(allocator, f.items);
                try code.append(allocator, isa.Insn.init(.ret, {}));
            }

            // resolve calls to openCall and do tail call optimization
            for (0..code.items.len) |i| {
                const insn = code.items[i];
                if (insn == .open_call) {
                    const name = insn.open_call;
                    const lbl = labels.get(name) orelse
                        return isa.programFrom(allocator, Insn.init(.not_found_error, name));

                    // replace this placeholder instruction with a normal call
                    var replace = isa.Insn.init(.call, lbl);
                    // if a call is immediately followed by a return, optimize to
                    // a jump for tail call optimization.
                    if (optimize.nextInsn(code.items[i + 1 ..])) |next| {
                        switch (next) {
                            .ret => {
                                replace = isa.Insn.init(.jump, lbl);
                                // remove the return instruction if there is no label referring to it
                                const ret_hl = optimize.nextInsnLabel(code.items[i + 1 ..]);
                                const had_label = ret_hl[1];
                                if (!had_label) {
                                    code.items[i + 1 + ret_hl[0]] = .nop;
                                }
                            },
                            else => {},
                        }

                        // perform the replacement of the opencall by either a call or jump
                        code.items[i] = replace;
                    }
                }
            }

            try code.append(allocator, lend.toInsn());

            return code;
        },
        .alt_slice, .seq_slice => unreachable,
        .seq => |n| {
            var l = try n.left.get(allocator).compile(allocator);
            errdefer l.deinit(allocator);
            var r = try n.right.get(allocator).compile(allocator);
            defer r.deinit(allocator);
            try l.appendSlice(allocator, r.items);
            return l;
        },
        .non_term => |n| {
            if (n.inlined) |sp| return sp.compile(allocator);
            return isa.programFrom(allocator, Insn.init(.open_call, n.name));
        },

        .alt => |n| {
            // optimization: if Left and Right are charsets/single chars, return the union
            if (n.left.* != .empty and n.right.* != .empty) {
                if (optimize.combine(n.left.get(allocator), n.right.get(allocator))) |set| {
                    return isa.programFrom(allocator, Insn.init(.set, set));
                }
            }

            var l = try n.left.get(allocator).compile(allocator);
            defer l.deinit(allocator);
            var r = try n.right.get(allocator).compile(allocator);
            defer r.deinit(allocator);

            const l1 = isa.Label.init();
            // optimization: if the right and left nodes are disjoint, we can use
            // NoChoice variants of the head-fail optimization instructions.
            var disjoint = false;
            var testinsn: isa.Insn = undefined;
            blk: {
                const linsn = optimize.nextInsn(l.items) orelse break :blk;
                const rinsn = optimize.nextInsn(r.items) orelse break :blk;

                switch (linsn) {
                    .set => |lt| {
                        switch (rinsn) {
                            .char => |rt| disjoint = !lt.isSet(rt),
                            else => {},
                        }
                        testinsn = .{ .test_set_no_choice = .{ .chars = lt, .lbl = l1 } };
                    },
                    .char => |lt| {
                        switch (rinsn) {
                            .char => |rt| disjoint = lt != rt,
                            .set => |rt| disjoint = !rt.isSet(lt),
                            else => {},
                        }
                        testinsn = .{ .test_char_no_choice = .{ .byte = lt, .lbl = l1 } };
                    },
                    else => {},
                }
            }

            const l2 = isa.Label.init();
            // std.log.debug("compile(.alt) l1={} l2={}", .{ l1.id, l2.id });
            var code = try Program.initCapacity(allocator, l.items.len + r.items.len + 5);

            if (disjoint) {
                code.appendAssumeCapacity(testinsn);
                code.appendSliceAssumeCapacity(l.items[1..]);
                code.appendAssumeCapacity(Insn.init(.jump, l2));
            } else {
                code.appendAssumeCapacity(Insn.init(.choice, l1));
                code.appendSliceAssumeCapacity(l.items);
                code.appendAssumeCapacity(Insn.init(.commit, l2));
            }
            code.appendAssumeCapacity(l1.toInsn());
            code.appendSliceAssumeCapacity(r.items);
            code.appendAssumeCapacity(l2.toInsn());
            return code;
        },
        .star => |n| {
            switch (n.*) {
                .class => |nn| {
                    return try isa.programFrom(allocator, Insn.init(.span, nn));
                },
                .memo => |t| {
                    // optimization: if the pattern we are repeating is a memoization
                    // entry, we should use special instructions to memoize it as a tree to
                    // get logarithmic saving when reparsing.
                    var sub = try t.patt.get(allocator).compile(allocator);
                    defer sub.deinit(allocator);
                    var code = try Program.initCapacity(allocator, sub.items.len + 11);
                    const l1 = isa.Label.init();
                    const l2 = isa.Label.init();
                    const l3 = isa.Label.init();
                    const no_jump = isa.Label.init();
                    pattern.memo_id += 1;

                    code.appendAssumeCapacity(l1.toInsn());
                    code.appendAssumeCapacity(Insn.init(.memo_tree_open, .{ .id = pattern.memo_id, .lbl = l3 }));
                    code.appendAssumeCapacity(Insn.init(.choice, l2));
                    code.appendSliceAssumeCapacity(sub.items);
                    code.appendAssumeCapacity(Insn.init(.commit, no_jump));
                    code.appendAssumeCapacity(no_jump.toInsn());
                    code.appendAssumeCapacity(Insn.init(.memo_tree_insert, {}));
                    code.appendAssumeCapacity(l3.toInsn());
                    code.appendAssumeCapacity(Insn.init(.memo_tree, {}));
                    code.appendAssumeCapacity(Insn.init(.jump, l1));
                    code.appendAssumeCapacity(l2.toInsn());
                    code.appendAssumeCapacity(Insn.init(.memo_tree_close, pattern.memo_id));
                    return code;
                },
                else => {},
            }
            var sub = try n.get(allocator).compile(allocator);
            defer sub.deinit(allocator);
            var code = try Program.initCapacity(allocator, sub.items.len + 4);
            var l1 = isa.Label.init();
            var l2 = isa.Label.init();
            code.appendAssumeCapacity(Insn.init(.choice, l2));
            code.appendAssumeCapacity(l1.toInsn());
            code.appendSliceAssumeCapacity(sub.items);
            code.appendAssumeCapacity(Insn.init(.partial_commit, l1));
            code.appendAssumeCapacity(l2.toInsn());
            return code;
        },
        .plus => |n| {
            const starp = Pattern{ .star = n.get(allocator) };
            var star = try starp.compile(allocator);
            defer star.deinit(allocator);
            var sub = try n.get(allocator).compile(allocator);
            defer sub.deinit(allocator);

            var code = try Program.initCapacity(allocator, sub.items.len + star.items.len);
            code.appendSliceAssumeCapacity(sub.items);
            code.appendSliceAssumeCapacity(star.items);
            return code;
        },
        .optional => |n| {
            switch (n.get(allocator).*) {
                .literal => |s| if (s.len == 1) {
                    const l1 = isa.Label.init();
                    return isa.programFromSlice(allocator, &.{
                        Insn.init(.test_char_no_choice, .{ .byte = s[0], .lbl = l1 }),
                        l1.toInsn(),
                    });
                },
                .class => |c| {
                    const l1 = isa.Label.init();
                    return isa.programFromSlice(allocator, &.{
                        Insn.init(.test_set_no_choice, .{ .chars = c, .lbl = l1 }),
                        l1.toInsn(),
                    });
                },
                else => {},
            }
            const a = Pattern{ .alt = .{
                .left = n.get(allocator),
                .right = Pattern.null_ptr,
            } };
            return a.compile(allocator);
        },
        .negative => |n| {
            var sub = try n.get(allocator).compile(allocator);
            defer sub.deinit(allocator);
            const l1 = isa.Label.init();
            var code = try isa.Program.initCapacity(allocator, sub.items.len + 3);
            code.appendAssumeCapacity(Insn.init(.choice, l1));
            code.appendSliceAssumeCapacity(sub.items);
            code.appendAssumeCapacity(Insn.init(.fail_twice, {}));
            code.appendAssumeCapacity(l1.toInsn());
            return code;
        },
        .positive => |n| {
            var sub = try n.get(allocator).compile(allocator);
            var code = try isa.Program.initCapacity(allocator, sub.items.len + 5);
            defer sub.deinit(allocator);
            const l1 = isa.Label.init();
            const l2 = isa.Label.init();
            code.appendAssumeCapacity(Insn.init(.choice, l1));
            code.appendSliceAssumeCapacity(sub.items);
            code.appendAssumeCapacity(Insn.init(.back_commit, l2));
            code.appendAssumeCapacity(Insn.init(.label, l1));
            code.appendAssumeCapacity(Insn.init(.fail, {}));
            code.appendAssumeCapacity(Insn.init(.label, l2));
            return code;
        },
        .not => |n| {
            const subn = n.get(allocator);
            if (subn.* == .class) {
                return isa.programFrom(allocator, Insn.init(.set, subn.class));
            } else {
                std.log.err("expected Not() pattern to simplify into a character class. found .{s}", .{@tagName(subn.*)});
                return error.InvalidNot;
            }
        },
        .cap => |n| {
            var sub = try n.patt.get(allocator).compile(allocator);
            defer sub.deinit(allocator);
            var code = try Program.initCapacity(allocator, sub.items.len + 2);

            var i: usize = 0;
            var back: u8 = 0;

            for (sub.items) |insn| {
                switch (insn) {
                    .char, .set => {
                        back += 1;
                    },
                    .any => |t| {
                        back += t;
                    },
                    else => break,
                }
                i += 1;
            }

            if (i == 0 or back >= 256) {
                try code.append(allocator, Insn.init(.capture_begin, n.id));
                i = 0;
            } else if (i == sub.items.len and back < 256) {
                try code.appendSlice(allocator, sub.items);
                try code.append(allocator, Insn.init(.capture_full, .{ .back = back, .id = n.id }));
                return code;
            } else {
                try code.appendSlice(allocator, sub.items[0..i]);
                try code.append(allocator, Insn.init(.capture_late, .{ .back = back, .id = n.id }));
            }
            try code.appendSlice(allocator, sub.items[i..]);
            try code.append(allocator, Insn.init(.capture_end, undefined));
            return code;
        },
        .no_cap => |n| {
            _ = n;
            unreachable;
        },
        .memo => |n| {
            _ = n;
            unreachable;
        },
        .check => |n| {
            const L1 = isa.Label.init();
            var sub = try n.patt.get(allocator).compile(allocator);
            defer sub.deinit(allocator);
            var code = try Program.initCapacity(allocator, sub.items.len + 3);
            code.appendAssumeCapacity(Insn.init(.check_begin, .{
                .id = n.id,
                .flag = n.flag,
            }));
            code.appendSliceAssumeCapacity(sub.items);
            code.appendAssumeCapacity(Insn.init(.check_end, n.checker));
            code.appendAssumeCapacity(L1.toInsn());
            return code;
        },
        .search => |n| {
            var set: Charset = undefined;
            var opt = false;

            var sub = try n.get(allocator).compile(allocator);
            defer sub.deinit(allocator);

            if (optimize.nextInsn(sub.items)) |next| {
                switch (next) {
                    .char => |t| {
                        set = Charset.initEmpty();
                        set.set(t);
                        set = set.complement();
                        opt = true;
                    },
                    .set => |t| {
                        // Heuristic: if the set is smaller than 10 chars, it
                        // is unlikely enough to match that we should consume all
                        // chars from the complement before continuing the search.
                        // The number 10 was arbitrarily chosen.
                        if (t.count() < 10) {
                            set = t.complement();
                            opt = true;
                        }
                    },
                    else => {},
                }
            }

            const rsearch = if (opt)
                pattern.GroupBuf(2, &.{
                    pattern.ZeroOrMore(pattern.Set(set)),
                    pattern.NonTerm("S"),
                })
            else
                pattern.NonTerm("S");

            const patt = pattern.SelectBuf(2, &.{
                n.get(allocator).*,
                pattern.GroupBuf(2, &.{ pattern.Any(1), rsearch }),
            });
            var g = try Pattern.rulesToGrammar(allocator, &.{.{ "S", patt }}, "S");
            defer g.deinit(allocator);
            return g.compile(allocator);
        },
        .repeat => |n| {
            _ = n;
            unreachable;
        },
        .class => |n| {
            return isa.programFrom(allocator, Insn.init(.set, n));
        },
        .char_fn => |n| {
            _ = n;
            unreachable;
        },
        .literal => |n| {
            if (n.len == 0) return error.InvalidLiteral;
            var code = try Program.initCapacity(allocator, n.len);
            for (n) |c|
                code.appendAssumeCapacity(Insn.init(.char, c));
            return code;
        },
        .dot => |n| {
            return isa.programFrom(allocator, Insn.init(.any, n));
        },
        .err => |n| {
            _ = n;
            unreachable;
        },
        .empty_op => |n| {
            _ = n;
            unreachable;
        },
        .empty, .null => {
            return Program{};
        },
        .escape => |n| {
            _ = n;
            unreachable;
        },
    }
    unreachable;
}

// Nodes with trees larger than this size will not be inlined.
const inline_threshold = 100;

fn inline_(p: Pattern, allocator: mem.Allocator) !bool {
    assert(p == .grammar);
    const g = p.grammar;
    const Sizes = std.StringHashMap(usize);
    var sizes = Sizes.init(allocator);
    defer sizes.deinit();
    const Leaves = std.StringHashMap(void);
    var leaves = Leaves.init(allocator);
    defer leaves.deinit();

    var iter = g.rules.iterator();
    while (iter.next()) |e| {
        const n = e.key_ptr.*;
        const sub = e.value_ptr;

        var size: usize = 0;
        var leaf = true;
        const W = struct {
            leaf: *bool,
            size: *usize,
            pub fn walk(w: *@This(), pat: Ptr) !void {
                // std.log.debug("walk {s} {} {}", .{ @tagName(pat.*), @enumToInt(pat.*), @enumToInt(Pattern.Tag.memo) });
                if (pat.* == .non_term and pat.non_term.inlined == null)
                    w.leaf.* = false;
                w.size.* += 1;
            }
        };
        var w = W{ .leaf = &leaf, .size = &size };
        try sub.walk(true, *W, &w);
        try sizes.put(n, size);
        if (leaf)
            try leaves.put(n, {});
    }

    var didInline = false;
    const W = struct {
        sizes: Sizes,
        leaves: Leaves,
        didInline: *bool,
        rules: Pattern.RuleMap,
        pub fn walk(w: *@This(), pat: Ptr) !void {
            if (pat.* == .non_term and pat.non_term.inlined == null) {
                // We only inline nodes if they are small enough and don't use
                // any non-terminals themselves.
                const name = pat.non_term.name;
                const sz = w.sizes.get(name) orelse return;
                if (sz < inline_threshold) {
                    _ = w.leaves.get(name) orelse return;
                    w.didInline.* = true;
                    pat.non_term.inlined = w.rules.getPtr(name);
                }
            }
        }
    };
    var w = W{ .sizes = sizes, .leaves = leaves, .didInline = &didInline, .rules = g.rules };

    var pmut = p;
    try pmut.walk(true, *W, &w);
    return didInline;
}
