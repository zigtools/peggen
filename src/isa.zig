const ParserGenerator = @import("ParserGenerator.zig");

/// openCall is a dummy instruction for resolving recursive function calls in
/// grammars.
const openCall = struct {
    name: []const u8,
    insn: Insn_ = .nop,
};

pub const Program = std.ArrayListUnmanaged(Insn);

pub const Insn = union(enum) {
    insn: Insn_,
    open_call: openCall,

    pub fn init(i: Insn_) Insn {
        return .{ .insn = i };
    }
    pub fn initOpenCall(name: []const u8, i: Insn_) Insn {
        return .{ .open_call = .{ .name = name, .insn = i } };
    }

    pub fn format(i: Insn, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (i) {
            .open_call => |n| try writer.print("name={s}", .{n.name}),
            .insn => |ii| try writer.print("{}", .{ii}),
        }
    }
};

pub const Insn_ = union(enum) {
    label: Label,
    char: Char,
    jump: Jump,
    choice: Choice,
    call: Call,
    commit: Commit,
    ret: Return,
    fail: Fail,
    set: Set,
    any: Any,
    partial_commit: PartialCommit,
    span: Span,
    back_commit: BackCommit,
    fail_twice: FailTwice,
    empty: Empty,
    test_char: TestChar,
    test_char_no_choice: TestCharNoChoice,
    test_set: TestSet,
    test_set_no_choice: TestSetNoChoice,
    test_any: TestAny,
    end: End,
    nop: Nop,
    memo_open: MemoOpen,
    memo_close: MemoClose,
    memo_tree_open: MemoTreeOpen,
    memo_tree_insert: MemoTreeInsert,
    memo_tree: MemoTree,
    memo_tree_close: MemoTreeClose,
    capture_begin: CaptureBegin,
    capture_late: CaptureLate,
    capture_end: CaptureEnd,
    capture_full: CaptureFull,
    check_begin: CheckBegin,
    check_end: CheckEnd,
    err: Error,

    pub fn format(i: Insn_, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        if (i != .label) {
            _ = try writer.write(@tagName(i));
        }
        switch (i) {
            .set => |n| {
                var iter = n.chars.iterator(.{});
                _ = try writer.write("=");
                while (iter.next()) |item|
                    try writer.print("{c}", .{@intCast(u8, item)});
            },
            .label => |n| try writer.print("{}:", .{n.id}),
            .jump => |j| try writer.print(" {}", .{j.lbl.id}),
            else => {},
        }
    }
};

const std = @import("std");
/// Label is used for marking a location in the instruction code with
/// a unique ID
pub const Label = struct {
    id: usize,

    var uniq_id: usize = 0;
    /// returns a label with a unique ID
    pub fn init() Label {
        uniq_id += 1;
        return .{
            .id = uniq_id,
        };
    }
    pub fn toInsn(l: Label) Insn {
        return Insn.init(.{ .label = l });
    }

    pub fn format(l: Label, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print(".id={}", .{l.id});
    }
};

/// Char consumes the next byte of the subject if it matches Byte and
/// fails otherwise.
pub const Char = struct {
    byte: u8,
};

/// Jump jumps to Lbl.
pub const Jump = struct {
    lbl: Label,
};

/// Choice pushes Lbl to the stack and if there is a failure the label will
/// be popped from the stack and jumped to.
pub const Choice = struct {
    lbl: Label,
};

/// Call pushes the next instruction to the stack as a return address and jumps
/// to Lbl.
pub const Call = struct {
    lbl: Label,
};

/// Commit jumps to Lbl and removes the top entry from the stack
pub const Commit = struct {
    lbl: Label,
};

/// Return pops a return address off the stack and jumps to it.
pub const Return = struct {};

/// Fail causes the instruction pointer to go to the fail state.
pub const Fail = struct {};

/// Set consumes the next byte of input if it is in the set of chars defined
/// by Chars.
pub const Set = struct {
    chars: ParserGenerator.Charset,
};

/// Any consumes the next N bytes and fails if that is not possible.
pub const Any = struct {
    n: u8,
};

/// PartialCommit modifies the backtrack entry on the top of the stack to
/// point to the current subject offset, and jumps to Lbl.
pub const PartialCommit = struct {
    lbl: Label,
};

/// Span consumes zero or more bytes in the set Chars. This instruction
/// never fails.
pub const Span = struct {
    chars: ParserGenerator.Charset,
};

/// BackCommit pops a backtrack entry off the stack, goes to the subject
/// position in the entry, and jumps to Lbl.
pub const BackCommit = struct {
    lbl: Label,
};

/// FailTwice pops an entry off the stack and sets the instruction pointer to
/// the fail state.
pub const FailTwice = struct {};

/// Empty makes a zero-width assertion according to the Op option. We use the
/// same zero-width assertions that are supported by Go's regexp package.
pub const Empty = struct {};

/// TestChar consumes the next byte if it matches Byte and jumps to Lbl
/// otherwise. If the consumption is possible, a backtrack entry referring
/// to Lbl and the subject position from before consumption is pushed to the
/// stack.
pub const TestChar = struct {
    byte: u8,
    lbl: Label,
};

/// TestCharNoChoice consumes the next byte if it matches Byte and jumps to Lbl
/// otherwise. No backtrack entry is pushed to the stack.
pub const TestCharNoChoice = struct {
    byte: u8,
    lbl: Label,
};

/// TestSet consumes the next byte if it is in the set Chars and jumps to
/// Lbl otherwise. If the consumption is possible, a backtrack entry referring
/// to Lbl and the subject position from before consumption is pushed to the
/// stack.
pub const TestSet = struct {
    chars: ParserGenerator.Charset,
    lbl: Label,
};

/// TestSetNoChoice is the same as TestSet but no backtrack entry is pushed to
/// the stack.
pub const TestSetNoChoice = struct {
    chars: ParserGenerator.Charset,
    lbl: Label,
};

/// TestAny consumes the next N bytes and jumps to Lbl if that is not possible.
/// If the consumption is possible, a backtrack entry referring to Lbl and
/// the subject position from before consumption is pushed to the stack.
pub const TestAny = struct {
    n: u8,
    lbl: Label,
};

/// End immediately completes the pattern as a match.
pub const End = struct {
    fail: bool,
};

/// Nop does nothing.
pub const Nop = struct {};

/// MemoOpen begins a memo entry at this position. It marks the pattern that is
/// being memoized with a unique ID for that pattern, and stores a label to
/// jump to if the pattern is found in the memoization table.
pub const MemoOpen = struct {
    lbl: Label,
    id: usize,
};

/// MemoClose completes a memoization entry and adds the entry into the memo
/// table if it meets certain conditions (size, or other heuristics).
pub const MemoClose = struct {};

/// MemoTreeOpen starts a memoization tree repetition routine.
pub const MemoTreeOpen = struct {
    lbl: Label,
    id: usize,
};

/// MemoTreeInsert performs insertion into the memoization table for the tree
/// memoization strategy.
pub const MemoTreeInsert = struct {};

/// MemoTree "tree-ifies" the current memoization entries on the stack.
pub const MemoTree = struct {};

/// MemoTreeClose completes the tree memoization routine.
pub const MemoTreeClose = struct {
    id: usize,
};

/// CaptureBegin begins capturing the given ID.
pub const CaptureBegin = struct {
    id: usize,
};

/// CaptureLate begins capturing the given ID at the current subject position
/// minus Back.
pub const CaptureLate = struct {
    back: u8,
    id: usize,
};

/// CaptureEnd completes an active capture.
pub const CaptureEnd = struct {
    id: usize,
};

/// CaptureFull begins a capture for the given ID at the current subject
/// position minus Back, and immediately completes the capture. This is
/// equivalent to CaptureLate Back ID; CaptureEnd.
pub const CaptureFull = struct {
    back: u8,
    id: usize,
};

/// CheckBegin marks the beginning position for a checker.
pub const CheckBegin = struct {
    id: usize,
    flag: usize,
};

/// CheckEnd records the end position of a checker and applies the checker to
/// determine if the match should fail.
pub const CheckEnd = struct {
    // checker: Checker,
};

/// Error logs an error message at the current position.
pub const Error = struct {
    message: []const u8,
};
