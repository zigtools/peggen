const Stream = @This();

index: usize = 0,
buffer: []const u8,

pub fn peek(self: *Stream, by: usize) error{EndOfStream}!u8 {
    if (self.index + by >= self.buffer.len) return error.EndOfStream;
    return self.buffer[self.index + by];
}

pub fn consume(self: *Stream, count: usize) error{EndOfStream}!void {
    if (self.index + count >= self.buffer.len) return error.EndOfStream;
    self.index += count;
}

pub fn sliceToEnd(self: *Stream) []const u8 {
    return self.buffer[self.index..];
}

pub fn sliceBy(self: *Stream, count: usize) error{EndOfStream}![]const u8 {
    if (self.index + count >= self.buffer.len) return error.EndOfStream;
    return self.buffer[self.index .. self.index + count];
}

pub fn read(self: *Stream, count: usize) error{EndOfStream}![]const u8 {
    const res = try self.sliceBy(count);
    try self.consume(count);
    return res;
}
