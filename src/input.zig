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

        pub fn refill(i: *Self, pos_: usize) !void {
            i.base = pos_;
            i.coff = 0;
            try i.r.seekTo(i.base);
            i.nchunk = try i.r.context.read(i.chunk[0..]);
            // std.debug.print("input.refill() i.nchunk={}, i.base={}, i.coff={}, i.furthest={}\n", .{ i.nchunk, i.base, i.coff, i.furthest });
        }

        pub fn pos(i: Self) isize {
            return @intCast(isize, i.base + i.coff);
        }

        /// returns the next byte in the stream or 'false' if there are no more
        /// bytes. Successive calls to Peek will return the same value unless there is a
        /// call to SeekTo or Advance in between.
        pub fn peek(i: *Self) ?u8 {
            const pos_ = i.base + i.coff;
            if (pos_ > i.furthest) i.furthest = pos_;
            return if (i.nchunk != 0) i.chunk[i.coff] else null;
        }

        /// moves the current read position to the desired read position. Returns
        /// true if the seek went to a valid location within the reader, and false
        /// otherwise. In other words, if seek returns true the next call to Peek will
        /// return a valid byte.
        pub fn seekTo(i: *Self, pos_: usize) !bool {
            // check if the seek position in within the current chunk and if so just
            // update the internal offset.
            const chunk_end = i.base + i.nchunk;
            if (pos_ < chunk_end and pos_ >= i.base) {
                i.coff = pos_ - i.base;
                return true;
            }

            // refill the cache (moves the base)
            try i.refill(pos_);
            return i.nchunk != 0;
        }

        /// assumes peek(n) has already happened. can be used in place of
        /// advance(n) when peek(n) has already happened.
        pub fn advanceAssume(i: *Self, n: isize) void {
            _ = i.advance(n) catch unreachable;
        }

        /// moves the offset forward by 'n' bytes. Returns true if the advance
        /// was successful (n chars were successfully skipped) and false otherwise. Note
        /// that even if Advance returns true the next call to Peek may return false if
        /// the advance went to the exact end of the data.
        pub fn advance(i: *Self, n: isize) !bool {
            if (i.nchunk == 0) return false;

            i.coff += @intCast(usize, n);
            if (i.coff > i.nchunk) {
                try i.refill(i.base + i.coff);
                return false;
            } else if (i.coff == i.nchunk) {
                try i.refill(i.base + i.coff);
            }
            return true;
        }

        pub fn readAt(i: *Self, buf: []u8, pos_: isize) !usize {
            try i.r.seekTo(@intCast(u64, pos_));
            return i.r.context.read(buf);
        }

        /// returns a slice of the reader corresponding to the range [low:high).
        pub fn slice(i: *Self, low: isize, high: isize, buf: []u8) ![]const u8 {
            if (buf.len < high - low) return error.OutOfMemory;
            const n = try i.readAt(buf[0..@intCast(usize, high - low)], low);
            return buf[0..n];
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
