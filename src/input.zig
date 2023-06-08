const std = @import("std");

/// represents the input data and is an efficient wrapper of io.ReaderAt
/// which provides a nicer API, avoids repeated interface function calls, and
/// uses a cache for buffered reading.
/// An Input also tracks the index of the furthest byte that has been read.
pub fn Input(comptime R: type) type {
    return struct {
        r: R,

        // cached data.
        chunk: [std.mem.page_size]u8,
        b: [1]u8,
        // size of the cache.
        nchunk: usize,
        // the position within the reader that the chunk starts at.
        base: usize,
        // the offset within the chunk we are reading at.
        coff: usize,
        // the furthest position we have read.
        furthest: usize,

        pub const Self = @This();
        pub fn refill(i: *Self, pos: usize) !void {
            i.base = pos;
            i.coff = 0;
            try i.r.seekTo(i.base);
            i.nchunk = try i.r.context.read(&i.chunk);
        }
    };
}

// NewInput creates a new Input wrapper for the io.ReaderAt.
pub fn input(seekable_stream: anytype) !Input(@TypeOf(seekable_stream)) {
    var i = Input(@TypeOf(seekable_stream)){
        .r = seekable_stream,
        .chunk = undefined,
        .b = undefined,
        .nchunk = 0,
        .base = 0,
        .coff = 0,
        .furthest = 0,
    };
    try i.refill(i.base);
    return i;
}
