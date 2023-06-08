const std = @import("std");

// An Entry represents a memoized parse result. It stores the non-terminal
// memoized, the start position of the parse result, the length, and the number
// of characters examined to make the parse determination. If the length is -1,
// the non-terminal failed to match at this location (but still may have
// examined a non-zero number of characters).
pub const Entry = struct {
    length: usize,
    examined: usize,
    count: usize,
    captures: Capture.List = .{},
    pos: usize,
};

const tNode = 0;
const tDummy = 1;

pub const Capture = struct {
    id: u32,
    typ: u32,
    off: usize,
    length: usize,
    ment: ?Entry = null,
    children: List = .{},

    pub const List = std.ArrayListUnmanaged(Capture);

    pub fn initNode(id: usize, start: usize, length: usize, children: List) Capture {
        return Capture{
            .id = @intCast(u32, id),
            .typ = tNode,
            .off = start,
            .length = length,
            .children = children,
        };
    }

    pub fn initDummy(start: usize, length: usize, children: List) Capture {
        return .{
            .id = 0,
            .typ = tDummy,
            .off = start,
            .length = length,
            .children = children,
        };
    }
};

pub const Table = std.AutoHashMapUnmanaged([2]usize, Entry);
