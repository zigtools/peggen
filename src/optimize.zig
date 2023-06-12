const std = @import("std");
const mem = std.mem;
const pattern = @import("pattern.zig");
const Ptr = pattern.Pattern.Ptr;
const isa = @import("isa.zig");

pub fn get(p: Ptr, allocator: mem.Allocator) Ptr {
    switch (p.*) {
        .non_term => |n| {
            if (n.inlined) |x| return x;
        },
        .alt_slice, .seq_slice => unreachable,
        .alt => |t| {
            const l = t.left.get(allocator);
            const r = t.right.get(allocator);
            if (l.* == .empty) return l;

            if (r.* == .empty) {
                r.* = .{ .optional = l };
                return r.get(allocator);
            }

            // Combine the left and right sides of an alternation into a class node
            // if possible.
            if (combine(l, r)) |set| {
                l.* = .{ .class = set };
                return l;
            }
        },
        .optional => |t| {
            // Optional of a Kleene star is unnecessary and we can remove the
            // optional.
            if (t.* == .star) return t.star;
        },
        .seq => |t| {
            // optimize use of empty: `a ""` and `"" a` are just `a`.
            const l = t.left.get(allocator);
            const r = t.right.get(allocator);
            if (r.* == .empty) return l;
            if (l.* == .empty) return r;

            // This optimizes patterns like `![a-z] .`. Instead of using a not
            // predicate in this case, we can just complement the set and use a
            // class node.
            const nn = if (l.* == .negative)
                l.negative
            else
                return p;

            var set: pattern.Charset = undefined;
            switch (nn.get(allocator).*) {
                .literal => |str| {
                    if (str.len != 1) return p;
                    set = pattern.Charset.initEmpty();
                    set.set(str[0]);
                },
                .class => |lt| set = lt,
                else => return p,
            }

            switch (r.*) {
                .dot => |rt| if (rt == 1) {
                    r.* = .{ .class = set.complement() };
                    return r;
                },
                .class => |rt| {
                    r.* = .{ .class = rt.differenceWith(set) };

                    return r;
                },
                .literal => |str| if (str.len == 1) {
                    var res = pattern.Charset.initEmpty();
                    res.set(str[0]);
                    r.* = .{ .class = res.differenceWith(set) };
                    return r;
                },
                else => {},
            }
        },
        .not => |n| switch (n.get(allocator).*) {
            .class => |class| {
                n.deinit(allocator);
                allocator.destroy(n);
                p.* = .{ .class = class.complement() };
                return p;
            },
            else => {},
        },
        else => {},
    }
    return p;
}

pub fn combine(p1: Ptr, p2: Ptr) ?pattern.Charset {
    switch (p1.*) {
        .literal => |str| {
            std.log.debug("combine lit", .{});
            if (str.len != 1) return null;
            switch (p2.*) {
                .class => |n| {
                    std.log.debug("combine lit 1", .{});
                    var s = n;
                    s.set(str[0]);
                    return s;
                },
                .literal => |str2| {
                    std.log.debug("combine lit 2", .{});
                    if (str2.len != 1) return null;
                    var s = pattern.Charset.initEmpty();
                    s.set(str[0]);
                    s.set(str2[0]);
                    return s;
                },

                else => {},
            }
        },
        .class => |n| {
            std.log.debug("combine class", .{});
            switch (p2.*) {
                .class => |n2| {
                    std.log.debug("combine class 1", .{});
                    var s = n;
                    s.setUnion(n2);
                    return s;
                },
                .literal => |str| {
                    std.log.debug("combine class 2", .{});
                    if (str.len != 1) return null;
                    var s = n;
                    s.set(str[0]);
                    return s;
                },

                else => {},
            }
        },
        else => {},
    }
    std.log.debug("combine lit null", .{});
    return null;
}

// Returns the next instruction in p, skipping labels and nops.
// If false is returned, there is no next instruction.
pub fn nextInsn(p: []const isa.Insn) ?isa.Insn {
    for (p) |insn| {
        switch (insn) {
            .label, .nop => {},
            else => return insn,
        }
    }

    return null;
}

// Returns the index of the next instruction and if there was a label before
// it.
pub fn nextInsnLabel(p: []const isa.Insn) struct { usize, bool } {
    var hadLabel = false;
    for (0..p.len) |i| {
        const insn = p[i];
        switch (insn) {
            .nop => {},
            .label => hadLabel = true,
            else => return .{ i, hadLabel },
        }
    }

    return .{ std.math.maxInt(usize), hadLabel };
}

// Optimize performs some optimization passes on the code in p. In particular
// it performs head-fail optimization and jump replacement.
pub fn optimize(allocator: mem.Allocator, p: *isa.Program) !void {
    // map from label to index in code
    var labels = std.AutoHashMap(isa.Label, usize).init(allocator);
    defer labels.deinit();
    for (p.items, 0..) |insn, i| {
        switch (insn) {
            .label => |n| try labels.put(n, i),
            else => {},
        }
    }
    std.log.debug("optimize p.items.len={}", .{p.items.len});
    for (p.items[0 .. p.items.len - 1], 0..) |insn, i| {
        // head-fail optimization: if we find a choice instruction immediately
        // followed (no label) by Char/Set/Any, we can replace with the
        // dedicated instruction TestChar/TestSet/TestAny.
        std.log.debug("optimize() insn={}", .{insn});
        if (insn != .choice) continue;
        const ch = insn.choice;
        switch (p.items[i + 1]) {
            .char => |t| {
                p.items[i] = isa.Insn.init(.test_char, .{
                    .byte = t,
                    .lbl = ch,
                });
                p.items[i + 1] = isa.Insn.init(.nop, {});
            },
            .set => |t| {
                p.items[i] = isa.Insn.init(.test_set, .{
                    .chars = t,
                    .lbl = ch,
                });
                p.items[i + 1] = isa.Insn.init(.nop, {});
            },
            .any => |t| {
                p.items[i] = isa.Insn.init(.test_any, .{
                    .n = t,
                    .lbl = ch,
                });
                p.items[i + 1] = isa.Insn.init(.nop, {});
            },
            else => {},
        }

        // jump optimization: if we find a jump to another control flow
        // instruction, we can replace the current jump directly with the
        // target instruction.{
        if (insn == .jump) {
            if (nextInsn(p.items[labels.get(insn.jump).?..])) |next_insn| {
                switch (next_insn) {
                    .partial_commit,
                    .back_commit,
                    .commit,
                    .jump,
                    .ret,
                    .fail,
                    .fail_twice,
                    .end,
                    => p.items[i] = next_insn,
                    else => {},
                }
            }
        }
    }
}
