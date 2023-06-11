const std = @import("std");
const mem = std.mem;
const pattern = @import("pattern.zig");
const Vm = @This();
const memo = @import("memo.zig");
const input = @import("input.zig");
const isa = @import("isa.zig");
const tree = @import("tree.zig");
const Interval = memo.Interval;

/// list of charsets
sets: std.ArrayListUnmanaged(pattern.Charset) = .{},
/// list of error messages
errors: std.ArrayListUnmanaged([]const u8) = .{},
/// list of checker functions
checkers: std.ArrayListUnmanaged(isa.Checker) = .{},
/// the encoded instructions
insns: std.ArrayListUnmanaged(u8) = .{},
allocator: mem.Allocator,

pub fn deinit(vm: *Vm) void {
    vm.sets.deinit(vm.allocator);
    vm.errors.deinit(vm.allocator);
    vm.insns.deinit(vm.allocator);
    vm.checkers.deinit(vm.allocator);
}

pub const ParseError = struct {
    message: []const u8,
    pos: usize,

    pub const List = std.ArrayListUnmanaged(ParseError);
    pub fn format(e: ParseError, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("ParseError: {s}", .{e.message});
    }
};

const ExecResult = struct {
    bool,
    usize,
    ?memo.Capture,
    ParseError.List,
};

pub const StackBacktrack = struct {
    ip: usize,
    off: usize,
    pub fn init(ip: usize, off: usize) StackBacktrack {
        return .{ .ip = ip, .off = off };
    }
};

pub const StackMemo = struct {
    id: u16 = 0,
    pos: usize = 0,
    count: usize = 0,
};

pub const StackEntry = struct {
    type: Type,
    // we could use a union to avoid the space cost but I have found this
    // doesn't impact performance and the space cost itself is quite small
    // because the stack is usually small.
    ret: usize = 0, // stackRet is reused for .check
    btrack: StackBacktrack,
    memo: StackMemo = .{}, // stackMemo is reused for .capt
    capt: std.ArrayListUnmanaged(memo.Capture) = .{},

    pub const Type = enum(u8) {
        ret,
        btrack,
        memo,
        memo_tree,
        capt,
        check,
    };

    pub fn deinit(se: *StackEntry, allocator: mem.Allocator) void {
        for (se.capt.items) |*capt| capt.deinit(allocator);
        se.capt.deinit(allocator);
    }

    pub fn addCapt(se: *StackEntry, allocator: mem.Allocator, capt: []const memo.Capture) !void {
        try se.capt.appendSlice(allocator, capt);
    }
};

pub const Stack = struct {
    entries: std.ArrayListUnmanaged(StackEntry) = .{},
    capt: std.ArrayListUnmanaged(memo.Capture) = .{},
    capt_arena: mem.Allocator,

    pub fn deinit(s: *Stack, allocator: mem.Allocator) void {
        for (s.entries.items) |*se| se.deinit(allocator);
        s.entries.deinit(allocator);
    }

    pub fn addCapt(s: *Stack, capt: []const memo.Capture) !void {
        if (s.entries.items.len == 0) {
            try s.capt.appendSlice(s.capt_arena, capt);
        } else {
            try s.entries.items[(s.entries.items.len) - 1].addCapt(s.capt_arena, capt);
        }
    }

    pub fn propCapt(s: *Stack, allocator: mem.Allocator) !void {
        if (s.entries.items.len == 0) return;

        const top = s.entries.items[(s.entries.items.len) - 1];
        if (top.capt.items.len > 0) {
            if (s.entries.items.len == 1) {
                try s.capt.appendSlice(allocator, top.capt.items);
            } else {
                try s.entries.items[(s.entries.items.len) - 2].addCapt(allocator, top.capt.items);
            }
        }
    }

    pub fn reset(
        s: *Stack,
    ) void {
        s.capt.clearRetainingCapacity();
        // need to complete remake the slice so that the underlying captures can be
        // released to the garbage collector if the user has no references to them
        // (unused stack entries shouldn't keep references to those captures).
        s.entries.clearRetainingCapacity();
    }

    pub fn push(s: *Stack, allocator: mem.Allocator, ent: StackEntry) !void {
        try s.entries.append(allocator, ent);
    }

    pub fn drop(s: *Stack, propagate: bool) !void {
        if (try s.pop(propagate)) |ent|
            ent.deinit(s.capt_arena);
    }
    // propagate marks whether captures should be propagated up the stack.
    pub fn pop(s: *Stack, propagate: bool) !?*StackEntry {
        if (s.entries.items.len == 0)
            return null;

        const ret = &s.entries.items[s.entries.items.len - 1];
        s.entries.items.len -= 1;

        // For non-capture entries, propagate the captures upward.
        // For capture entries, we create a new node with the corresponding
        // children, and this is manually handled by the caller.
        if (propagate)
            try s.addCapt(ret.capt.items);

        return ret;
    }

    pub fn peek(s: *Stack) ?*StackEntry {
        return s.peekn(0);
    }

    pub fn peekn(s: *Stack, n: usize) ?*StackEntry {
        if (s.entries.items.len <= n) {
            return null;
        }
        return &s.entries.items[s.entries.items.len - n - 1];
    }

    pub fn pushRet(s: *Stack, allocator: mem.Allocator, r: usize) !void {
        try s.push(allocator, .{
            .type = .ret,
            .ret = r,
            .btrack = undefined,
        });
    }

    pub fn pushBacktrack(s: *Stack, allocator: mem.Allocator, b: StackBacktrack) !void {
        try s.push(allocator, .{
            .type = .btrack,
            .btrack = b,
        });
    }

    pub fn pushMemo(s: *Stack, allocator: mem.Allocator, m: StackMemo) !void {
        try s.push(allocator, .{
            .type = .memo,
            .memo = m,
        });
    }

    pub fn pushMemoTree(s: *Stack, allocator: mem.Allocator, m: StackMemo) !void {
        try s.push(allocator, .{
            .type = .memo_tree,
            .memo = m,
            .btrack = undefined,
        });
    }

    pub fn pushCapt(s: *Stack, allocator: mem.Allocator, m: StackMemo) !void {
        try s.push(allocator, .{
            .type = .capt,
            .memo = m,
            .btrack = undefined,
        });
    }

    pub fn pushCheck(s: *Stack, allocator: mem.Allocator, m: StackMemo) !void {
        // std.log.debug("Stack.pushCheck() m={}", .{m});
        try s.push(allocator, .{
            .type = .check,
            .memo = m,
            .btrack = undefined,
        });
    }
};

pub fn init() Vm {
    return .{};
}

// returns the size in bytes of the encoded version of this instruction
fn size(insn: isa.Insn) usize {
    var s: usize = 0;
    switch (insn) {
        .label, .nop => return 0,
        .check_begin => s += 4,
        else => |n| s += if (n.isJumpType()) 4 else 2,
    }

    // handle instructions with extra args
    switch (insn) {
        .memo_open,
        .memo_tree_open,
        .memo_tree_close,
        .capture_begin,
        .capture_late,
        .capture_full,
        .test_char,
        .test_char_no_choice,
        .test_set,
        .test_set_no_choice,
        .test_any,
        .err,
        .check_begin,
        .check_end,
        => s += 2,
        else => {},
    }
    // std.log.debug("size({})={}", .{ insn, s });
    return s;
}

/// base instruction set
pub const BaseInsn = enum(u8) {
    char,
    jump,
    choice,
    call,
    commit,
    ret,
    fail,
    set,
    any,
    partial_commit,
    span,
    back_commit,
    fail_twice,
    empty,
    test_char,
    test_char_no_choice,
    test_set,
    test_set_no_choice,
    test_any,
    end,
    nop,
    capture_begin,
    capture_late,
    capture_end,
    capture_full,
    check_begin,
    check_end,
    memo_open,
    memo_close,
    memo_tree_open,
    memo_tree_insert,
    memo_tree,
    memo_tree_close,
    err,
    none,

    pub fn from(byte: u8) BaseInsn {
        return @intToEnum(BaseInsn, byte);
    }

    pub fn fromInsn(insn: isa.Insn) BaseInsn {
        return switch (insn) {
            .open_call => unreachable,
            .char => .char,
            .jump => .jump,
            .choice => .choice,
            .call => .call,
            .commit => .commit,
            .ret => .ret,
            .fail => .fail,
            .set => .set,
            .any => .any,
            .partial_commit => .partial_commit,
            .span => .span,
            .back_commit => .back_commit,
            .fail_twice => .fail_twice,
            .empty => .empty,
            .test_char => .test_char,
            .test_char_no_choice => .test_char_no_choice,
            .test_set => .test_set,
            .test_set_no_choice => .test_set_no_choice,
            .test_any => .test_any,
            .end => .end,
            .nop => .nop,
            .capture_begin => .capture_begin,
            .capture_late => .capture_late,
            .capture_end => .capture_end,
            .capture_full => .capture_full,
            .check_begin => .check_begin,
            .check_end => .check_end,
            .memo_open => .memo_open,
            .memo_close => .memo_close,
            .memo_tree_open => .memo_tree_open,
            .memo_tree_insert => .memo_tree_insert,
            .memo_tree => .memo_tree,
            .memo_tree_close => .memo_tree_close,
            .err, .not_found_error => .err,
            .label => .none,
        };
    }

    fn code(i: BaseInsn) u8 {
        return @enumToInt(i);
    }
};

// Encode transforms a program into VM bytecode.
pub fn encode(allocator: mem.Allocator, insns: isa.Program) !Vm {
    var code = Vm{ .allocator = allocator };
    errdefer code.deinit();

    var bcount: usize = 0;
    var labels = std.AutoHashMap(isa.Label, usize).init(allocator);
    defer labels.deinit();
    for (insns.items) |insn| {
        switch (insn) {
            .nop => {},
            .label => |n| try labels.put(n, bcount),
            else => bcount += size(insn),
        }
    }

    var iter = labels.iterator();
    while (iter.next()) |e|
        std.log.debug("encode() label {}: {}", .{ e.key_ptr.id, e.value_ptr.* });

    var args = std.ArrayList(u8).init(allocator);
    defer args.deinit();

    for (insns.items) |insn| {
        args.items.len = 0;
        // std.log.debug("encode {}", .{insn});
        switch (insn) {
            .label, .nop, .open_call => continue,
            .char => |n| {
                try args.appendSlice(&.{n});
            },
            .jump => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n).?));
            },
            .choice => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n).?));
            },
            .call => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n).?));
            },
            .commit => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n).?));
            },
            .ret => {},
            .fail => {},
            .set => |n| {
                try args.append(encodeU8(try code.addSet(allocator, n)));
            },
            .any => |n| {
                try args.appendSlice(&.{n});
            },
            .partial_commit => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n).?));
            },
            .span => |n| {
                try args.append(encodeU8(try code.addSet(allocator, n)));
            },
            .back_commit => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n).?));
            },
            .fail_twice => {},
            .empty => |n| {
                try args.appendSlice(&.{n});
            },
            .test_char => |n| {
                try args.append(n.byte);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_char_no_choice => |n| {
                try args.append(n.byte);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_set => |n| {
                try args.append(encodeU8(try code.addSet(allocator, n.chars)));
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_set_no_choice => |n| {
                try args.append(encodeU8(try code.addSet(allocator, n.chars)));
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_any => |n| {
                try args.append(n.n);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .capture_begin => |n| {
                try args.appendSlice(&encodeU16(n));
            },
            .capture_end => {},
            .capture_late => |n| {
                try args.append(n.back);
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .capture_full => |n| {
                try args.append(n.back);
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .memo_open => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .memo_close => {},
            .memo_tree_open => |n| {
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .memo_tree_insert => {},
            .memo_tree => {},
            .memo_tree_close => |n| {
                try args.appendSlice(&encodeI16(@intCast(isize, n)));
            },
            .check_begin => |n| {
                try args.appendSlice(&encodeU16(n.id));
                try args.appendSlice(&encodeU16(n.flag));
            },
            .check_end => |n| {
                try args.appendSlice(&encodeU24(try code.addChecker(
                    allocator,
                    n,
                )));
            },
            .err => |n| {
                try args.appendSlice(&encodeU24(try code.addError(allocator, n)));
            },
            .not_found_error => |n| {
                const m = try std.fmt.allocPrint(allocator, "non-terminal '{s}' not found", .{n});
                try args.appendSlice(&encodeU24(try code.addError(allocator, m)));
            },
            .end => |n| {
                try args.appendSlice(&.{encodeBool(n.fail)});
            },
            // else => std.debug.panic("invalid instruction during encoding: {}", .{insn}),
        }

        const l = code.insns.items.len;
        const op = BaseInsn.fromInsn(insn);
        try code.insns.append(allocator, op.code());

        // need padding to align the args if they are divisible by 16 bits
        if (args.items.len % 2 == 0) {
            try code.insns.append(allocator, 0);
        }

        try code.insns.appendSlice(allocator, args.items);
        std.log.debug("encoded {} {s} new insns={}", .{ insn, @tagName(op), std.fmt.fmtSliceHexLower(code.insns.items[l..]) });
    }
    try code.insns.appendSlice(allocator, &.{ BaseInsn.code(.end), 0 });

    return code;
}

fn encodeU8(x: usize) u8 {
    return @intCast(u8, x);
}

fn encodeI8(x: isize) u8 {
    return @bitCast(u8, @intCast(i8, x));
}

fn encodeU16(x_: usize) [2]u8 {
    const x = @intCast(u16, x_);
    var b: [2]u8 = undefined;
    mem.writeIntLittle(u16, &b, x);
    return b;
}

fn encodeI16(x_: isize) [2]u8 {
    const x = @intCast(i16, x_);
    var b: [2]u8 = undefined;
    mem.writeIntLittle(u16, &b, @bitCast(u16, x));
    return b;
}

fn encodeU24(x: usize) [3]u8 {
    _ = std.math.cast(u24, x) orelse unreachable;
    var b: [4]u8 = undefined;
    const ii1 = @truncate(u16, (x >> 16) & 0xff);
    const ii2 = @truncate(u16, x);
    // std.log.debug("encodeU24({}) i1={} i2={}", .{ x, ii1, ii2 });
    mem.writeIntBig(u16, b[0..2], ii1);
    mem.writeIntLittle(u16, b[2..], ii2);
    return b[1..4].*;
}

fn encodeLabel(x: usize) [3]u8 {
    return encodeU24(x);
}

fn encodeBool(b: bool) u8 {
    return @boolToInt(b);
}

// Adds the set to the code's list of charsets, and returns the index it was
// added at. If there are duplicate charsets, this may not actually insert
// the new charset.
fn addSet(code: *Vm, allocator: mem.Allocator, set: pattern.Charset) !usize {
    for (code.sets.items, 0..) |s, i| {
        if (set.eql(s)) return i;
    }

    const len = code.sets.items.len;
    try code.sets.append(allocator, set);
    return len;
}

fn addError(code: *Vm, allocator: mem.Allocator, msg: []const u8) !usize {
    for (code.errors.items, 0..) |s, i| {
        if (mem.eql(u8, msg, s)) return i;
    }

    const len = code.errors.items.len;
    try code.errors.append(allocator, msg);
    return len;
}

fn addChecker(code: *Vm, allocator: mem.Allocator, checker: isa.Checker) !usize {
    const len = code.checkers.items.len;
    try code.checkers.append(allocator, checker);
    return len;
}

/// Exec executes the parsing program this virtual machine was created with. It
/// returns whether the parse was a match, the last position in the subject
/// string that was matched, and any captures that were created.
/// 'captures_arena' is used only for captures
pub fn exec(
    vm: *Vm,
    seekable_stream: anytype,
    memtbl: *memo.Table,
    captures_arena: mem.Allocator,
) !ExecResult {
    // TODO improve memory strategy. i wasn't able to to prevent leaks
    // when captures were allocated with vm.allocator.  i added Stack.drop()
    // which fixed some leaks.  if we want to fix more, perhaps instances of
    // of Stack.pop() need to also be audited and the result deinit().
    var st = Stack{ .capt_arena = captures_arena };
    defer st.deinit(vm.allocator);

    var src = try input.input(seekable_stream);

    return vm.execImpl(0, &st, &src, memtbl, null);
}

pub fn sz(i: BaseInsn) u8 {
    return insn_sizes[@enumToInt(i)];
}

pub const insn_sizes = std.enums.directEnumArray(BaseInsn, u8, 0, .{
    // base instruction set
    .char = 2,
    .ret = 2,
    .fail = 2,
    .set = 2,
    .any = 2,
    .span = 2,
    .fail_twice = 2,
    .end = 2,
    .nop = 0,
    .empty = 2,
    .capture_begin = 4,
    .capture_late = 4,
    .capture_end = 2,
    .capture_full = 4,
    .memo_close = 2,
    .memo_tree_insert = 2,
    .memo_tree = 2,
    .memo_tree_close = 4,
    .check_begin = 6,
    .check_end = 4,
    .err = 4,

    // jumps
    .jump = 4,
    .choice = 4,
    .call = 4,
    .commit = 4,
    .partial_commit = 4,
    .back_commit = 4,
    .test_char = 6,
    .test_char_no_choice = 6,
    .test_set = 6,
    .test_set_no_choice = 6,
    .test_any = 6,
    .memo_open = 6,
    .memo_tree_open = 6,
    .none = undefined,
});

fn memoize(
    allocator: mem.Allocator,
    id: usize,
    pos: usize,
    mlen: isize,
    count: usize,
    mcapt: ?*memo.Capture.List,
    src_: anytype,
    memtbl_: *memo.Table,
    iv: ?Interval,
) !void {
    if (iv != null) {
        if (mcapt) |capt| capt.clearRetainingCapacity();
    }
    const mexam = @max(src_.furthest, src_.pos()) - pos + 1;
    try memtbl_.put(allocator, id, pos, mlen, mexam, count, if (mcapt) |c| c.* else .{});
}

fn execImpl(vm: *Vm, ip_: usize, st: *Stack, src: anytype, memtbl: *memo.Table, intrvl: ?Interval) !ExecResult {
    const idata = vm.insns;
    var ip = ip_;
    if (ip >= idata.items.len) {
        return .{ true, 0, memo.Capture.initDummy(0, 0, .{}), .{} };
    }

    var caprange: ?Interval = null;

    if (intrvl) |i| {
        caprange = i;
        // Apply an edit that clears all memoized entries in the interval
        // we are capturing. This ensures that we find all captures in the
        // requested interval.
        try memtbl.applyEdit(vm.allocator, .{
            .start = i.low,
            .end = i.high,
            .len = i.high - i.low,
        });
    }

    var success = true;
    var errs = ParseError.List{};

    loop: while (true) {
        var fail = false;
        const op = idata.items[ip];
        const insn = BaseInsn.from(op);
        std.log.debug("ip={} op={}/.{s}", .{ ip, op, @tagName(insn) });
        switch (insn) {
            .end => {
                const fail_ = decodeU8(idata.items[ip + 1 ..]);
                success = fail_ != 1;
                break :loop;
            },

            .char => {
                blk: {
                    if (src.peek()) |in| {
                        const b = decodeU8(idata.items[ip + 1 ..]);
                        // std.log.debug("char b={c} in={c}", .{ b, in });
                        if (b == in) {
                            _ = try src.advance(1);
                            ip += sz(.char);
                            break :blk;
                        }
                    }
                    fail = true;
                }
            },
            .jump => {
                const lbl = decodeU24(idata.items[ip + 1 ..]);
                ip = lbl;
            },
            .choice => {
                const lbl = decodeU24(idata.items[ip + 1 ..]);
                try st.pushBacktrack(vm.allocator, StackBacktrack.init(lbl, src.pos()));
                ip += sz(.choice);
            },
            .call => {
                const lbl = decodeU24(idata.items[ip + 1 ..]);
                try st.pushRet(vm.allocator, ip + sz(.call));
                ip = lbl;
            },
            .commit => {
                const lbl = decodeU24(idata.items[ip + 1 ..]);
                try st.drop(true);
                ip = lbl;
            },
            .ret => {
                blk: {
                    if (try st.pop(true)) |ent| {
                        if (ent.type == .ret) {
                            ip = ent.ret;
                            break :blk;
                        }
                    }
                    @panic("Return failed");
                }
            },
            .fail => {
                fail = true;
            },
            .set => {
                const set = decodeSet(idata.items[ip + 1 ..], vm.sets.items);
                blk: {
                    if (src.peek()) |in| {
                        // std.log.debug("set in='{c}' set.isSet(in)={} set={}", .{ in, set.isSet(in), pattern.CharsetFmt.init(set) });
                        if (set.isSet(in)) {
                            _ = try src.advance(1);
                            ip += sz(.set);
                            break :blk;
                        }
                    }
                    fail = true;
                }
            },
            .any => {
                const n = decodeU8(idata.items[ip + 1 ..]);
                if (try src.advance(n)) {
                    ip += sz(.any);
                } else {
                    fail = true;
                }
            },
            .partial_commit => {
                const lbl = decodeU24(idata.items[ip + 1 ..]);
                const ment = st.peek();
                blk: {
                    if (ment) |ent| {
                        if (ent.type == .btrack) {
                            ent.btrack.off = src.pos();
                            try st.propCapt(vm.allocator);
                            ent.capt.items.len = 0;
                            ip = lbl;
                            break :blk;
                        }
                    }
                    @panic("PartialCommit failed");
                }
            },
            .span => {
                const set = decodeSet(idata.items[ip + 1 ..], vm.sets.items);
                while (true) {
                    const in = src.peek() orelse break;
                    if (!set.isSet(in)) break;
                    _ = try src.advance(1);
                }
                ip += sz(.span);
            },
            .back_commit => {
                const err_msg = "BackCommit failed";
                const lbl = decodeU24(idata.items[ip + 1 ..]);
                const ent = try st.pop(true) orelse
                    @panic(err_msg);
                if (ent.type == .btrack) {
                    std.debug.assert(try src.seekTo(ent.btrack.off));
                    ip = lbl;
                } else {
                    @panic(err_msg);
                }
            },
            .fail_twice => {
                try st.drop(false);
                fail = true;
            },
            .test_char => {
                blk: {
                    const lbl = decodeU24(idata.items[ip + 3 ..]);
                    const b = decodeU8(idata.items[ip + 2 ..]);
                    if (src.peek()) |in| {
                        if (in == b) {
                            try st.pushBacktrack(vm.allocator, StackBacktrack.init(lbl, src.pos()));
                            _ = try src.advance(1);
                            ip += sz(.test_char);
                            break :blk;
                        }
                    }
                    ip = lbl;
                }
            },
            .test_char_no_choice => {
                blk: {
                    const b = decodeU8(idata.items[ip + 2 ..]);
                    if (src.peek()) |in| {
                        if (in == b) {
                            _ = try src.advance(1);
                            ip += sz(.test_char_no_choice);
                            break :blk;
                        }
                    }
                    const lbl = decodeU24(idata.items[ip + 3 ..]);
                    ip = lbl;
                }
            },
            .test_set => {
                const lbl = decodeU24(idata.items[ip + 3 ..]);
                const set = decodeSet(idata.items[ip + 2 ..], vm.sets.items);
                blk: {
                    if (src.peek()) |in| {
                        if (set.isSet(in)) {
                            try st.pushBacktrack(vm.allocator, StackBacktrack.init(lbl, src.pos()));
                            _ = try src.advance(1);
                            ip += sz(.test_set);
                            break :blk;
                        }
                    }
                    ip = lbl;
                }
            },
            .check_begin => {
                const id = decodeI16(idata.items[ip + 2 ..]);
                const flag = decodeI16(idata.items[ip + 4 ..]);
                try st.pushCheck(vm.allocator, .{
                    .id = @intCast(u16, id),
                    .count = @intCast(u16, flag),
                    .pos = src.pos(),
                });
                ip += sz(.check_begin);
            },
            .check_end => {
                const err_msg = "check end needs check stack entry";
                const ent = try st.pop(true) orelse
                    @panic(err_msg);
                if (ent.type != .check)
                    @panic(err_msg);
                const checkid = decodeU24(idata.items[ip + 1 ..]);
                const checker = vm.checkers.items[checkid];

                const id = @intCast(u32, ent.memo.id);
                const flag = @intCast(u32, ent.memo.count);
                const func = @ptrCast(isa.CheckerFn, checker.func);
                var buf: [0x100]u8 = undefined;
                const n = func(checker.ptr, try src.slice(ent.memo.pos, src.pos(), &buf), src, id, flag);
                if (n == -1) {
                    fail = true;
                } else {
                    _ = try src.advance(@bitCast(usize, n));
                }

                ip += sz(.check_end);
            },
            .memo_tree_open => {
                const lbl = decodeU24(idata.items[ip + 1 ..]);
                const id = decodeU16(idata.items[ip + 4 ..]);

                if (memtbl.get(id, src.pos())) |ment| {
                    if (ment.length == -1)
                        fail = true;

                    try st.pushMemoTree(vm.allocator, .{
                        .id = id,
                        .pos = src.pos(),
                        .count = ment.count,
                    });

                    try st.addCapt(ment.captures.items);
                    _ = try src.advance(@intCast(usize, ment.length));
                    _ = src.peek();
                    ip = lbl;
                } else {
                    try st.pushMemoTree(vm.allocator, .{
                        .id = id,
                        .pos = src.pos(),
                    });
                    ip += sz(.memo_tree_open);
                }
            },
            .capture_late => {
                const back = decodeU8(idata.items[ip + 1 ..]);
                const id = decodeU16(idata.items[ip + 2 ..]);
                try st.pushCapt(vm.allocator, .{
                    .id = id,
                    .pos = src.pos() - back,
                });
                ip += sz(.capture_late);
            },
            .capture_full => {
                const back = decodeU8(idata.items[ip + 1 ..]);
                const id = decodeU16(idata.items[ip + 2 ..]);
                const pos = src.pos();

                if (overlaps(intrvl, pos - back, pos)) {
                    const tmp = if (caprange) |c| c else Interval{ .low = 0, .high = 0 };
                    caprange = .{
                        .low = @min(tmp.low, pos - back),
                        .high = @max(tmp.high, pos),
                    };
                    const capt = memo.Capture.initNode(id, pos - back, back, .{});
                    try st.addCapt(&.{capt});
                }

                ip += sz(.capture_full);
            },
            .capture_end => {
                const err_msg = "CaptureEnd did not find capture entry";
                const ent = try st.pop(false) orelse
                    @panic(err_msg);

                if (ent.type != .capt)
                    @panic(err_msg);

                const end = src.pos();
                if (overlaps(intrvl, ent.memo.pos, end)) {
                    const tmp = if (caprange) |c| c else Interval{ .low = 0, .high = 0 };
                    caprange = .{
                        .low = @min(tmp.low, ent.memo.pos),
                        .high = @max(tmp.high, end),
                    };
                    const capt = memo.Capture.initNode(ent.memo.id, ent.memo.pos, end - ent.memo.pos, ent.capt);
                    try st.addCapt(&.{capt});
                }
                ip += sz(.capture_end);
            },
            .memo_tree_insert => {
                const err_msg = "no memo entry on stack";
                const ent = st.peek() orelse
                    @panic(err_msg);
                if (ent.type != .memo_tree)
                    @panic(err_msg);
                const mlen = src.pos() - ent.memo.pos;
                ent.memo.count += 1;
                try memoize(
                    vm.allocator,
                    ent.memo.id,
                    ent.memo.pos,
                    @intCast(isize, mlen),
                    ent.memo.count,
                    &ent.capt,
                    src,
                    memtbl,
                    intrvl,
                );
                ip += sz(.memo_tree_insert);
            },
            .memo_tree => {
                var seen: usize = 0;
                var accum: usize = 0;
                while (true) {
                    const top = st.peekn(seen) orelse break;
                    const next = st.peekn(seen + 1) orelse break;

                    if (top.type != .memo_tree or next.type != .memo_tree)
                        break;

                    seen += 1;
                    accum += top.memo.count;

                    if (accum < next.memo.count) continue;

                    for (1..seen) |_|
                        try st.drop(true);
                    if (try st.pop(false)) |ent| {
                        // next is now top of stack
                        if (ent.capt.items.len > 0) {
                            if (intrvl == null) {
                                const dummy = memo.Capture.initDummy(ent.memo.pos, src.pos() - ent.memo.pos, ent.capt);
                                try st.addCapt(&.{dummy});
                            } else {
                                try st.addCapt(ent.capt.items);
                            }
                        }
                    }

                    next.memo.count = accum + next.memo.count;
                    const mlen = src.pos() - next.memo.pos;
                    try memoize(
                        vm.allocator,
                        next.memo.id,
                        next.memo.pos,
                        @intCast(isize, mlen),
                        next.memo.count,
                        &next.capt,
                        src,
                        memtbl,
                        intrvl,
                    );

                    accum = 0;
                    seen = 0;
                }
                ip += sz(.memo_tree);
            },
            .memo_tree_close => {
                const id = decodeI16(idata.items[ip + 2 ..]);
                while (st.peek()) |p| {
                    if (p.type == .memo_tree and p.memo.id == id)
                        try st.drop(true);
                }
                ip += sz(.memo_tree_close);
            },
            else => std.debug.panic("Invalid opcode .{s}", .{@tagName(BaseInsn.from(op))}),
        }

        if (fail) {
            while (true) {
                std.log.debug("fail={} stack.len={}", .{ fail, st.entries.items.len });
                const stack_entry = (try st.pop(false)) orelse {
                    // match failed
                    return .{ false, src.pos(), null, errs };
                };
                std.log.debug("stack_entry.type=.{s}", .{@tagName(stack_entry.type)});

                switch (stack_entry.type) {
                    .btrack => {
                        // std.log.debug("btrack={}", .{stack_entry.btrack});
                        ip = stack_entry.btrack.ip;
                        _ = try src.seekTo(stack_entry.btrack.off);
                        stack_entry.capt.clearRetainingCapacity();
                        break;
                    },
                    .memo => {
                        // Mark this position in the memoTable as a failed match
                        try memoize(vm.allocator, stack_entry.memo.id, stack_entry.memo.pos, -1, 0, null, src, memtbl, intrvl);

                        stack_entry.capt.clearRetainingCapacity();
                    },
                    .ret, .capt, .check => {
                        stack_entry.capt.clearRetainingCapacity();
                    },
                    else => {},
                }
            }
        }
    }
    if (intrvl != null) {
        const caprg = caprange orelse unreachable;
        return .{ success, src.pos(), memo.Capture.initDummy(caprg.low, caprg.high - caprg.low, st.capt), errs };
    }
    return .{ success, src.pos(), memo.Capture.initDummy(0, src.pos(), st.capt), errs };
}

fn decodeU8(b: []const u8) u8 {
    return b[0];
}

fn decodeI8(b: []const u8) i8 {
    return @bitCast(i8, b[0]);
}

fn decodeU16(b: []const u8) u16 {
    return mem.readIntLittle(u16, b[0..2]);
}

fn decodeI16(b: []const u8) i16 {
    return mem.readIntLittle(i16, b[0..2]);
}

fn decodeU24(b: []const u8) u32 {
    const ii1 = @as(u32, decodeU8(b));
    const ii2 = @as(u32, decodeU16(b[1..]));
    return (ii1 << 16) | ii2;
}

fn decodeSet(b: []const u8, sets: []const pattern.Charset) pattern.Charset {
    const i = decodeU8(b);
    return sets[i];
}

fn overlaps(i_: ?Interval, low2: usize, high2: usize) bool {
    const i = i_ orelse return true;
    return i.low < high2 and i.high > low2;
}
