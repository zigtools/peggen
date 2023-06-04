const std = @import("std");
const Stream = @This();

index: usize = 0,
input: []const u8,
output: std.io.FixedBufferStream([]u8),
flags: Flags = Flags.initEmpty(),

pub const Flags = std.enums.EnumSet(Flag);
pub const Flag = enum { suppress_output };

/// 'output' should be the same size as 'input' but may be smaller when parse
///  results are expected to be smaller than input
pub fn init(input: []const u8, output: []u8) Stream {
    return .{ .input = input, .output = std.io.fixedBufferStream(output) };
}

pub fn peek(self: *Stream, count: usize) ?u8 {
    if (self.index + count >= self.input.len) return null;
    return self.input[self.index + count];
}

pub fn eof(self: *Stream) bool {
    return (self.index >= self.input.len);
}

pub fn read(self: *Stream, count: usize) error{EndOfStream}![]const u8 {
    const new_index = self.index + count;
    if (new_index > self.input.len) return error.EndOfStream;
    defer self.index = new_index;
    return self.input[self.index..new_index];
}

pub fn readByte(self: *Stream) error{EndOfStream}!u8 {
    if (self.index >= self.input.len) return error.EndOfStream;
    defer self.index += 1;
    return self.input[self.index];
}

pub fn write(self: *Stream, bytes: []const u8) error{NoSpaceLeft}!usize {
    return if (!self.flags.contains(.suppress_output))
        self.output.write(bytes)
    else
        0;
}

pub fn writeByte(self: *Stream, byte: u8) error{NoSpaceLeft}!void {
    if (!self.flags.contains(.suppress_output))
        try self.output.writer().writeByte(byte);
}

pub const State = struct { index: usize, output_pos: usize };
/// helper for saving state
pub fn checkpoint(self: Stream) State {
    return .{ .index = self.index, .output_pos = self.output.pos };
}

/// helper for restoring state
pub fn restore(self: *Stream, state: State) void {
    self.index = state.index;
    self.output.pos = state.output_pos;
}
