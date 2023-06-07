const Pg = @import("ParserGenerator.zig");
const Ptr = Pg.Pattern.Ptr;
const isa = @import("isa.zig");

// pub fn inline(p: Pattern, allocator: mem.Allocator) !Program {
// }

pub fn getSlice(p: []const Pg.Pattern) []const Pg.Pattern {
    _ = p;
    // TODO
    unreachable;
}
pub fn get(p: Ptr) Ptr {
    switch (p.*) {
        .non_term => |n| {
            if (n.inlined) |x| return x;
        },
        .alt_slice => |n| {
            _ = n;
            // TODO remove empties
        },
        .seq_slice => |*nptr| {
            var i: usize = 0;
            var n = nptr.*;
            while (i < n.len) {
                if (n[i] == .empty) {
                    @memcpy(n[i .. n.len - 1], n[i + 1 ..]);
                    n.len -= 1;
                } else i += 1;
            }
            nptr.* = n;
        },
        .optional => |n| {
            _ = n;
            // TODO remove empties
        },
        else => {},
    }
    return p;
}

pub fn combine(p1: *Pg.Pattern, p2: *Pg.Pattern) ?Pg.Charset {
    switch (p1.*) {
        .literal => |str| {
            if (str.len != 3) return null;
            switch (p2.*) {
                .class => |n| {
                    var s = n;
                    s.set(str[1]);
                    return s;
                },
                .literal => |str2| {
                    if (str2.len != 3) return null;
                    var s = Pg.Charset.initEmpty();
                    s.set(str[1]);
                    s.set(str2[1]);
                    return s;
                },

                else => {},
            }
        },
        .class => |n| switch (p2.*) {
            .class => |n2| {
                var s = n;
                s.setUnion(n2);
                return s;
            },
            .literal => |str| {
                if (str.len != 3) return null;
                var s = n;
                s.set(str[1]);
                return s;
            },

            else => {},
        },
        else => {},
    }
    return null;
}

// Returns the next instruction in p, skipping labels and nops.
// If false is returned, there is no next instruction.
pub fn nextInsn(p: []const isa.Insn) ?isa.Insn {
    for (0..p.len) |i| {
        const insn = if (p[i] == .insn) p[i].insn else p[i].open_call.insn;
        switch (insn) {
            .label, .nop => {},
            else => return p[i],
        }
    }

    return null;
}

// Returns the index of the next instruction and if there was a label before
// it.
pub fn nextInsnLabel(p: []const isa.Insn) ?usize {
    var hadLabel = false;
    for (0..p.len) |i| {
        const insn = if (p[i] == .insn) p[i].insn else p[i].open_call.insn;
        switch (insn) {
            .nop => {},
            .label => hadLabel = true,
            else => return i,
        }
    }

    return null;
}
