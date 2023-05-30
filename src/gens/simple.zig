const std = @import("std");
const Stream = @import("../Stream.zig");

pub const Context = struct {
    error_index: ?usize = null,

    fn setErrorIndex(ctx: *Context, idx: usize) void {
        if (ctx.error_index == null) ctx.error_index = idx;
    }
};

pub const SimpleParserGenerator = struct {
    pub fn String(comptime string: []const u8) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;

                if (!std.mem.eql(u8, try stream.read(string.len), string)) {
                    stream.index = current;
                    ctx.setErrorIndex(stream.index);
                    return error.ValidateFailure;
                }
            }
        };
    }

    pub fn Positive(comptime entry: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;
                defer stream.index = current;

                entry.validate(stream, ctx) catch |err| switch (err) {
                    error.ValidateFailure => {
                        ctx.setErrorIndex(stream.index);
                        return error.ValidateFailure;
                    },
                    else => return err,
                };
            }
        };
    }
    pub fn Negative(comptime entry: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;
                defer stream.index = current;

                entry.validate(stream, ctx) catch |err| switch (err) {
                    error.ValidateFailure => return,
                    else => return err,
                };

                ctx.setErrorIndex(stream.index);
                return error.ValidateFailure;
            }
        };
    }

    pub fn Optional(comptime entry: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;

                entry.validate(stream, ctx) catch |err| switch (err) {
                    error.ValidateFailure => {
                        stream.index = current;
                        ctx.setErrorIndex(stream.index);
                        return;
                    },
                    else => return err,
                };
            }
        };
    }
    pub fn ZeroOrMore(comptime entry: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                while (true) {
                    const current = stream.index;

                    entry.validate(stream, ctx) catch |err| switch (err) {
                        error.ValidateFailure => {
                            stream.index = current;
                            ctx.setErrorIndex(stream.index);
                            return;
                        },
                        else => return err,
                    };
                }
            }
        };
    }
    pub fn OneOrMore(comptime entry: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                var i: usize = 0;
                while (true) : (i += 1) {
                    const current = stream.index;

                    entry.validate(stream, ctx) catch |err| switch (err) {
                        error.ValidateFailure => {
                            stream.index = current;

                            if (i == 0) {
                                ctx.setErrorIndex(stream.index);
                                return error.ValidateFailure;
                            }

                            return;
                        },
                        else => return err,
                    };
                }
            }
        };
    }

    pub fn Any() type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                stream.consume(1) catch |err| switch (err) {
                    error.EndOfStream => {
                        ctx.setErrorIndex(stream.index);
                        return error.ValidateFailure;
                    },
                    else => return err,
                };
            }
        };
    }
    pub fn AnyOf(comptime entries: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                var set = comptime b: {
                    var cset = std.bit_set.StaticBitSet(256).initEmpty();
                    for (entries) |val| {
                        cset.setValue(val, true);
                    }
                    break :b cset;
                };

                const current = stream.index;
                if (!set.isSet((try stream.read(1))[0])) {
                    stream.index = current;
                    ctx.setErrorIndex(stream.index);
                    return error.ValidateFailure;
                }
            }
        };
    }
    pub fn NoneOf(comptime entries: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                comptime var set = std.bit_set.StaticBitSet(256).initEmpty();
                inline for (entries) |val| {
                    set.setValue(val, true);
                }

                const current = stream.index;
                if (set.isSet((try stream.read(1))[0])) {
                    stream.index = current;
                    ctx.setErrorIndex(stream.index);
                    return error.ValidateFailure;
                }
            }
        };
    }

    pub fn Group(comptime entries: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                inline for (entries) |entry| {
                    try entry.validate(stream, ctx);
                }
            }
        };
    }
    pub fn Select(comptime entries: anytype) type {
        return struct {
            pub fn validate(stream: *Stream, ctx: *Context) anyerror!void {
                inline for (entries) |entry| {
                    var valid = false;
                    entry.validate(stream, ctx) catch |err| switch (err) {
                        error.ValidateFailure => {
                            valid = true;
                        },
                        else => return err,
                    };
                    if (!valid)
                        return;
                }

                ctx.setErrorIndex(stream.index);
                return error.ValidateFailure;
            }
        };
    }
};
