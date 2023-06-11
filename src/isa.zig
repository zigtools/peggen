const std = @import("std");
const mem = std.mem;
const ParserGenerator = @import("ParserGenerator.zig");
const input = @import("input.zig");

pub const Program = std.ArrayListUnmanaged(Insn);

pub inline fn programFrom(allocator: mem.Allocator, i: Insn) !Program {
    var result = try Program.initCapacity(allocator, 1);
    result.appendAssumeCapacity(i);
    return result;
}

pub fn programFromSlice(allocator: mem.Allocator, is: []const Insn) !Program {
    var result = try Program.initCapacity(allocator, is.len);
    result.appendSliceAssumeCapacity(is);
    return result;
}

pub const Insn = union(enum) {
    /// openCall is a dummy instruction for resolving recursive function calls in
    /// grammars.
    open_call: []const u8,
    /// Label is used for marking a location in the instruction code with
    /// a unique ID
    label: Label,
    /// Char consumes the next byte of the subject if it matches Byte and
    /// fails otherwise.
    char: u8,
    /// Jump jumps to Lbl.
    jump: Label,
    /// Choice pushes Lbl to the stack and if there is a failure the label will
    /// be popped from the stack and jumped to.
    choice: Label,
    /// Call pushes the next instruction to the stack as a return address and jumps
    /// to Lbl.
    call: Label,
    /// Commit jumps to Lbl and removes the top entry from the stack
    commit: Label,
    /// Return pops a return address off the stack and jumps to it.
    ret,
    /// Fail causes the instruction pointer to go to the fail state.
    fail,
    /// Set consumes the next byte of input if it is in the set of chars defined
    /// by Chars.
    set: ParserGenerator.Charset,
    /// Any consumes the next N bytes and fails if that is not possible.
    any: u8,
    /// PartialCommit modifies the backtrack entry on the top of the stack to
    /// point to the current subject offset, and jumps to Lbl.
    partial_commit: Label,
    /// Span consumes zero or more bytes in the set Chars. This instruction
    /// never fails.
    span: ParserGenerator.Charset,
    /// BackCommit pops a backtrack entry off the stack, goes to the subject
    /// position in the entry, and jumps to Lbl.
    back_commit: Label,
    /// FailTwice pops an entry off the stack and sets the instruction pointer to
    /// the fail state.
    fail_twice,
    /// Empty makes a zero-width assertion according to the Op option. We use the
    /// same zero-width assertions that are supported by Go's regexp package.
    empty: u8,
    /// TestChar consumes the next byte if it matches Byte and jumps to Lbl
    /// otherwise. If the consumption is possible, a backtrack entry referring
    /// to Lbl and the subject position from before consumption is pushed to the
    /// stack.
    test_char: ByteLabel,
    /// TestCharNoChoice consumes the next byte if it matches Byte and jumps to Lbl
    /// otherwise. No backtrack entry is pushed to the stack.
    test_char_no_choice: ByteLabel,
    /// TestSet consumes the next byte if it is in the set Chars and jumps to
    /// Lbl otherwise. If the consumption is possible, a backtrack entry referring
    /// to Lbl and the subject position from before consumption is pushed to the
    /// stack.
    test_set: CharsLabel,
    /// TestSetNoChoice is the same as TestSet but no backtrack entry is pushed to
    /// the stack.
    test_set_no_choice: CharsLabel,
    /// TestAny consumes the next N bytes and jumps to Lbl if that is not possible.
    /// If the consumption is possible, a backtrack entry referring to Lbl and
    /// the subject position from before consumption is pushed to the stack.
    test_any: TestAny,
    /// End immediately completes the pattern as a match.
    end: End,
    /// Nop does nothing.
    nop,
    /// MemoOpen begins a memo entry at this position. It marks the pattern that is
    /// being memoized with a unique ID for that pattern, and stores a label to
    /// jump to if the pattern is found in the memoization table.
    memo_open: LabelId,
    /// MemoClose completes a memoization entry and adds the entry into the memo
    /// table if it meets certain conditions (size, or other heuristics).
    memo_close,
    /// MemoTreeOpen starts a memoization tree repetition routine.
    memo_tree_open: LabelId,
    /// MemoTreeInsert performs insertion into the memoization table for the tree
    /// memoization strategy.
    memo_tree_insert,
    /// MemoTree "tree-ifies" the current memoization entries on the stack.
    memo_tree,
    /// MemoTreeClose completes the tree memoization routine.
    memo_tree_close: usize,
    /// CaptureBegin begins capturing the given ID.
    capture_begin: usize,
    /// CaptureLate begins capturing the given ID at the current subject position
    /// minus Back.
    capture_late: BackId,
    /// CaptureEnd completes an active capture.
    capture_end: usize,
    /// CaptureFull begins a capture for the given ID at the current subject
    /// position minus Back, and immediately completes the capture. This is
    /// equivalent to CaptureLate Back ID; CaptureEnd.
    capture_full: BackId,
    /// CheckBegin marks the beginning position for a checker.
    check_begin: CheckBegin,
    /// CheckEnd records the end position of a checker and applies the checker to
    /// determine if the match should fail.
    check_end: Checker,
    /// Error logs an error message at the current position.
    err: []const u8,
    /// A NotFoundError means a a non-terminal was not found during grammar
    /// compilation.
    not_found_error: []const u8,

    pub const Tag = std.meta.Tag(Insn);
    pub fn init(comptime tag: Tag, payload: anytype) Insn {
        return @unionInit(Insn, @tagName(tag), payload);
    }

    pub fn format(i: Insn, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        if (i != .label) {
            _ = try writer.write(@tagName(i));
        }
        switch (i) {
            .set => |n| try writer.print("{}", .{ParserGenerator.CharsetFmt.init(n)}),
            .label => |n| try writer.print("l{}:", .{n.id}),
            .jump => |j| try writer.print(" l{}", .{j.id}),
            .choice => |n| try writer.print(" l{}", .{n.id}),
            .char => |n| try writer.print(" '{c}'", .{n}),

            else => {},
        }
    }

    /// returns true if insn has a label
    pub fn isJumpType(insn: Insn) bool {
        switch (insn) {
            inline else => |_, tag| {
                const T = std.meta.FieldType(Insn, tag);
                return comptime mem.indexOfScalar(
                    type,
                    &.{ Label, LabelId, ByteLabel, CharsLabel, TestAny },
                    T,
                ) != null;
            },
        }
    }
};

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
        return Insn.init(.label, l);
    }
};

pub const ByteLabel = struct {
    byte: u8,
    lbl: Label,
};

pub const CharsLabel = struct {
    chars: ParserGenerator.Charset,
    lbl: Label,
};

pub const TestAny = struct {
    n: u8,
    lbl: Label,
};

pub const End = struct {
    fail: bool,
};

pub const LabelId = struct {
    lbl: Label,
    id: usize,
};

pub const BackId = struct {
    back: u8,
    id: usize,
};

pub const CheckBegin = struct {
    id: usize,
    flag: usize,
};

pub const CheckerFnT = fn (
    checker: ?*anyopaque,
    b: []const u8,
    src: ?*anyopaque,
    id: u32,
    flag: u32,
) isize;

pub const CheckerFn = *const CheckerFnT;

pub const Checker = struct {
    ptr: ?*anyopaque,
    func: CheckerFn,
};

pub const Ref = enum(u8) {
    def,
    use,
    block,

    pub fn code(r: Ref) u8 {
        return @enumToInt(r);
    }
};

pub const BackReference = struct {
    symbols: std.AutoHashMapUnmanaged(usize, []const u8) = .{},
    buf: [0x100]u8 = undefined,
    allocator: mem.Allocator,

    pub fn deinit(br: *BackReference) void {
        var iter = br.symbols.iterator();
        while (iter.next()) |e|
            br.allocator.free(e.value_ptr.*);
        br.symbols.deinit(br.allocator);
    }

    pub const Stream = std.io.FixedBufferStream([]u8).SeekableStream;
    pub const Input = input.Input(Stream);
    pub fn check(self: ?*anyopaque, b: []const u8, src_: ?*anyopaque, id: u32, flag: u32) isize {
        const r = @ptrCast(*BackReference, @alignCast(@alignOf(*BackReference), self));
        const src = @ptrCast(*Input, @alignCast(@alignOf(*Input), src_));
        switch (@intToEnum(Ref, @intCast(u8, flag))) {
            .def => {
                // std.log.debug("BackReference.check() Def b={s}", .{b});
                const duped = r.allocator.dupe(u8, b) catch |e|
                    std.debug.panic("BackReference.check() error: {}", .{e});
                const gop = r.symbols.getOrPut(r.allocator, id) catch |e|
                    std.debug.panic("BackReference.check() error: {}", .{e});
                if (gop.found_existing) r.allocator.free(gop.value_ptr.*);
                gop.value_ptr.* = duped;
                return 0;
            },
            .use => {
                const back = r.symbols.get(id) orelse
                    std.debug.panic("BackReference.check() internal error: id={} not found", .{id});

                const n = src.readAt(r.buf[0..back.len], src.pos()) catch |e|
                    std.debug.panic("BackReference.check() error in src.readAt(): {}", .{e});
                // std.log.debug("BackReference.check() Use n={} read='{s}', back='{s}'", .{ n, r.buf[0..n], back });
                if (std.mem.eql(u8, r.buf[0..n], back)) {
                    return @bitCast(isize, n);
                }
                return -1;
            },
            .block => {},
        }
        return 0;
    }
};
