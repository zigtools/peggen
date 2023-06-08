const std = @import("std");
const mem = std.mem;
const Pg = @import("ParserGenerator.zig");
const Vm = @This();
const memo = @import("memo.zig");
const input = @import("input.zig");
const isa = @import("isa.zig");

/// list of charsets
sets: std.ArrayListUnmanaged(Pg.Charset) = .{},
/// list of error messages
errors: std.ArrayListUnmanaged([]const u8) = .{},
/// list of checker functions
checkers: void = {}, //: []isa.Checker
/// the encoded instructions
insns: std.ArrayListUnmanaged(u8) = .{},

pub const ParseError = struct {
    message: []const u8,
    pos: usize,
};

const ExecResult = struct {
    bool,
    isize,
    memo.Capture,
    []const ParseError,
};

pub const StackBacktrack = struct {
    ip: usize,
    off: usize,
};

pub const StackMemo = struct {
    id: u16,
    pos: usize,
    count: usize,
};

pub const StackEntry = struct {
    stype: u8,
    // we could use a union to avoid the space cost but I have found this
    // doesn't impact performance and the space cost itself is quite small
    // because the stack is usually small.
    ret: usize, // stackRet is reused for stCheck
    btrack: StackBacktrack,
    memo: StackMemo, // stackMemo is reused for stCapt
    capt: std.ArrayListUnmanaged(memo.Capture) = .{},
};

pub const Stack = struct {
    entries: std.ArrayListUnmanaged(StackEntry) = .{},
    capt: std.ArrayListUnmanaged(memo.Capture) = .{},
};

pub fn init() Vm {
    return .{};
}

// returns the size in bytes of the encoded version of this instruction
fn size(insn: isa.Insn) usize {
    var sz: usize = 0;
    switch (insn.inner()) {
        .label, .nop => return 0,
        .jump_type, .check_begin => sz += 4,
        else => sz += 2,
    }

    // handle instructions with extra args
    switch (insn.inner()) {
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
        => sz += 2,
        else => {},
    }

    return sz;
}

/// base instruction set
const BaseInsn = enum(u8) {
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
};

fn opcode(i: BaseInsn) u8 {
    return @enumToInt(i);
}

fn setArgs(args: *std.ArrayList(u8), bytes: []const u8) !void {
    args.items.len = 0;
    try args.appendSlice(bytes);
}

// Encode transforms a program into VM bytecode.
pub fn encode(allocator: mem.Allocator, insns: isa.Program) !Vm {
    var code = Vm{};

    var bcount: usize = 0;
    var labels = std.AutoHashMap(isa.Label, usize).init(allocator);
    for (insns.items) |insn| {
        switch (insn.inner()) {
            .nop => {},
            .label => |n| try labels.put(n, bcount),
            else => bcount += size(insn),
        }
    }

    for (insns.items) |insn| {
        var op: u8 = undefined;
        var args = std.ArrayList(u8).init(allocator);

        switch (insn.inner()) {
            .label, .nop => continue,
            .char => |n| {
                op = opcode(.char);
                try setArgs(&args, &.{n});
            },
            .jump => |n| {
                op = opcode(.jump);
                try setArgs(&args, &encodeLabel(labels.get(n).?));
            },
            .choice => |n| {
                op = opcode(.choice);
                try setArgs(&args, &encodeLabel(labels.get(n).?));
            },
            .call => |n| {
                op = opcode(.call);
                try setArgs(&args, &encodeLabel(labels.get(n).?));
            },
            .commit => |n| {
                op = opcode(.commit);
                try setArgs(&args, &encodeLabel(labels.get(n).?));
            },
            .ret => {
                op = opcode(.ret);
            },
            .fail => {
                op = opcode(.fail);
            },
            .set => |n| {
                op = opcode(.set);
                try setArgs(&args, &.{encodeU8(try code.addSet(allocator, n))});
            },
            .any => |n| {
                op = opcode(.any);
                try setArgs(&args, &.{n});
            },
            .partial_commit => |n| {
                op = opcode(.partial_commit);
                try setArgs(&args, &encodeLabel(labels.get(n).?));
            },
            .span => |n| {
                op = opcode(.span);
                try setArgs(&args, &.{encodeU8(try code.addSet(allocator, n))});
            },
            .back_commit => |n| {
                op = opcode(.back_commit);
                try setArgs(&args, &encodeLabel(labels.get(n).?));
            },
            .fail_twice => {
                op = opcode(.fail_twice);
            },
            .empty => |n| {
                op = opcode(.empty);
                try setArgs(&args, &.{n});
            },
            .test_char => |n| {
                op = opcode(.test_char);
                try args.append(n.byte);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_char_no_choice => |n| {
                op = opcode(.test_char_no_choice);
                try args.append(n.byte);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_set => |n| {
                op = opcode(.test_set);
                try args.append(encodeU8(try code.addSet(allocator, n.chars)));
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_set_no_choice => |n| {
                op = opcode(.test_set_no_choice);
                try args.append(encodeU8(try code.addSet(allocator, n.chars)));
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .test_any => |n| {
                op = opcode(.test_any);
                try args.append(n.n);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
            },
            .capture_begin => |n| {
                op = opcode(.capture_begin);
                try setArgs(&args, &encodeU16(n));
            },
            .capture_end => {
                op = opcode(.capture_end);
            },
            .capture_late => |n| {
                op = opcode(.capture_late);
                try args.append(n.back);
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .capture_full => |n| {
                op = opcode(.capture_full);
                try args.append(n.back);
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .memo_open => |n| {
                op = opcode(.memo_open);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .memo_close => {
                op = opcode(.memo_close);
            },
            .memo_tree_open => |n| {
                op = opcode(.memo_tree_open);
                try args.appendSlice(&encodeLabel(labels.get(n.lbl).?));
                try args.appendSlice(&encodeI16(@intCast(isize, n.id)));
            },
            .memo_tree_insert => {
                op = opcode(.memo_tree_insert);
            },
            .memo_tree => {
                op = opcode(.memo_tree);
            },
            .memo_tree_close => |n| {
                op = opcode(.memo_tree_close);
                try setArgs(&args, &encodeI16(@intCast(isize, n)));
            },
            .check_begin => |n| {
                op = opcode(.check_begin);
                try args.appendSlice(&encodeU16(n.id));
                try args.appendSlice(&encodeU16(n.flag));
            },
            .check_end => |n| {
                op = opcode(.check_end);
                try setArgs(&args, &encodeU24(try code.addChecker(allocator, n.checker)));
            },
            .err => |n| {
                op = opcode(.err);
                try setArgs(&args, &encodeU24(try code.addError(allocator, n)));
            },
            .end => |n| {
                op = opcode(.end);
                try setArgs(&args, &.{encodeBool(n.fail)});
            },
            else => std.debug.panic("invalid instruction during encoding: {}", .{insn}),
        }

        try code.insns.append(allocator, op);

        // need padding to align the args if they are divisible by 16 bits
        if (args.items.len % 2 == 0) {
            try code.insns.append(allocator, 0);
        }

        try code.insns.appendSlice(allocator, try args.toOwnedSlice());
    }
    try code.insns.appendSlice(allocator, &.{ opcode(.end), 0 });

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

fn encodeU24(x_: usize) [3]u8 {
    const x = @intCast(u24, x_);
    var b: [4]u8 = undefined;
    const ii1 = @truncate(u16, (x >> 16) & 0xff);
    const ii2 = @truncate(u16, x);
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
fn addSet(code: *Vm, allocator: mem.Allocator, set: Pg.Charset) !usize {
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

    const len = code.sets.items.len;
    try code.errors.append(allocator, msg);
    return len;
}

fn addChecker(code: *Vm, allocator: mem.Allocator, checker: isa.Checker) !usize {
    _ = allocator;
    _ = checker;
    _ = code;
    // code.Checkers = append(code.Checkers, checker)
    // return usize(len(code.Checkers) - 1)
    unreachable;
}

/// Exec executes the parsing program this virtual machine was created with. It
/// returns whether the parse was a match, the last position in the subject
/// string that was matched, and any captures that were created.
pub fn exec(vm: *Vm, reader: anytype, memtbl: memo.Table) !ExecResult {
    var st = Stack{};
    var src = try input.input(reader);

    return vm.execImpl(0, &st, &src, memtbl, null);
}

fn execImpl(vm: *Vm, ip: usize, st: *Stack, src: anytype, memtbl: memo.Table, interval: ?void) !ExecResult {
    const idata = vm.insns;

    if (ip >= idata.items.len) {
        return .{ true, 0, memo.Capture.initDummy(0, 0, .{}), &.{} };
    }

    _ = interval;
    _ = src;
    _ = st;
    _ = memtbl;
    unreachable;
}
