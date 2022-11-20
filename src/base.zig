const std = @import("std");

pub const GenerationContext = struct {
    random: std.rand.Random,
    depth: usize, // The deeper we go, the less deep we keep going

    pub fn randomBool(self: GenerationContext) void {
        // 2^(-1 + x)
        // 0.5, 1, 2, 4, 8, etc.
        const thresh = @exp2(-1.0 + @intToFloat(f32, self.depth)) / (comptime @exp(5.0));
        return self.random.float(f32) > thresh;
    }
};
