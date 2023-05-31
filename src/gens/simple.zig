const std = @import("std");
const Stream = @import("../Stream.zig");

pub const Context = struct {
    allocator: std.mem.Allocator,
    error_indices: std.ArrayListUnmanaged(usize) = .{},

    fn addError(ctx: *Context, stream: *Stream, rule: []const u8) void {
        _ = ctx;
        std.log.err("Rule {s} emitted an error at {d}", .{ rule, stream.index });
    }
};

pub const SimpleParserGenerator = struct {
    pub fn String(comptime rule: @TypeOf(.enum_literal), comptime expected: []const u8) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                const actual = stream.read(expected.len) catch |err| switch (err) {
                    error.EndOfStream => {
                        ctx.addError(stream, @tagName(rule));
                        return error.TraversalFailure;
                    },
                    else => return err,
                };

                std.log.info("{s} ?= {s}", .{ actual, expected });

                if (!std.mem.eql(u8, actual, expected)) {
                    ctx.addError(stream, @tagName(rule));
                    return error.TraversalFailure;
                }
            }
        };
    }

    pub fn Positive(comptime rule: @TypeOf(.enum_literal), comptime entry: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;
                defer stream.index = current;

                entry.traverse(stream, ctx) catch |err| switch (err) {
                    error.TraversalFailure => {
                        ctx.addError(stream, @tagName(rule));
                        return error.TraversalFailure;
                    },
                    else => return err,
                };
            }
        };
    }
    pub fn Negative(comptime rule: @TypeOf(.enum_literal), comptime entry: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;
                defer stream.index = current;

                entry.traverse(stream, ctx) catch |err| switch (err) {
                    error.TraversalFailure => return,
                    else => return err,
                };

                ctx.addError(stream, @tagName(rule));
                return error.TraversalFailure;
            }
        };
    }

    pub fn Optional(comptime rule: @TypeOf(.enum_literal), comptime entry: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;

                entry.traverse(stream, ctx) catch |err| switch (err) {
                    error.TraversalFailure => {
                        stream.index = current;
                        ctx.addError(stream, @tagName(rule));
                        return;
                    },
                    else => return err,
                };
            }
        };
    }
    pub fn ZeroOrMore(comptime rule: @TypeOf(.enum_literal), comptime entry: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                while (true) {
                    const current = stream.index;

                    entry.traverse(stream, ctx) catch |err| switch (err) {
                        error.TraversalFailure => {
                            stream.index = current;
                            ctx.addError(stream, @tagName(rule));
                            return;
                        },
                        else => return err,
                    };
                }
            }
        };
    }
    pub fn OneOrMore(comptime rule: @TypeOf(.enum_literal), comptime entry: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                var i: usize = 0;
                while (true) : (i += 1) {
                    const current = stream.index;

                    entry.traverse(stream, ctx) catch |err| switch (err) {
                        error.TraversalFailure => {
                            stream.index = current;

                            if (i == 0) {
                                ctx.addError(stream, @tagName(rule));
                                return error.TraversalFailure;
                            }

                            return;
                        },
                        else => return err,
                    };
                }
            }
        };
    }

    pub fn Any(comptime rule: @TypeOf(.enum_literal)) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                stream.consume(1) catch |err| switch (err) {
                    error.EndOfStream => {
                        ctx.addError(stream, @tagName(rule));
                        return error.TraversalFailure;
                    },
                    else => return err,
                };
            }
        };
    }
    pub fn AnyOf(comptime rule: @TypeOf(.enum_literal), comptime entries: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                var set = comptime b: {
                    var cset = std.bit_set.StaticBitSet(256).initEmpty();
                    for (entries) |val| {
                        cset.setValue(val, true);
                    }
                    break :b cset;
                };

                const char = (stream.read(1) catch |err| switch (err) {
                    error.EndOfStream => {
                        ctx.addError(stream, @tagName(rule));
                        return error.TraversalFailure;
                    },
                    else => return err,
                })[0];

                if (!set.isSet(char)) {
                    ctx.addError(stream, @tagName(rule));
                    return error.TraversalFailure;
                }
            }
        };
    }
    pub fn NoneOf(comptime rule: @TypeOf(.enum_literal), comptime entries: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                comptime var set = std.bit_set.StaticBitSet(256).initEmpty();
                inline for (entries) |val| {
                    set.setValue(val, true);
                }

                const char = (stream.read(1) catch |err| switch (err) {
                    error.EndOfStream => {
                        ctx.addError(stream, @tagName(rule));
                        return error.TraversalFailure;
                    },
                    else => return err,
                })[0];

                if (set.isSet(char)) {
                    ctx.addError(stream, @tagName(rule));
                    return error.TraversalFailure;
                }
            }
        };
    }

    pub fn Group(comptime rule: @TypeOf(.enum_literal), comptime entries: anytype) type {
        _ = rule;
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                inline for (entries) |entry| {
                    try entry.traverse(stream, ctx);
                }
            }
        };
    }
    pub fn Select(comptime rule: @TypeOf(.enum_literal), comptime entries: anytype) type {
        return struct {
            pub fn traverse(stream: *Stream, ctx: *Context) anyerror!void {
                const current = stream.index;

                inline for (entries) |entry| {
                    var valid = true;

                    entry.traverse(stream, ctx) catch |err| switch (err) {
                        error.TraversalFailure => {
                            stream.index = current;
                            valid = false;
                        },
                        else => return err,
                    };

                    if (valid)
                        return;
                }

                ctx.addError(stream, @tagName(rule));
                return error.TraversalFailure;
            }
        };
    }
};
