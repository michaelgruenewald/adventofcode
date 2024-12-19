const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, isize);
const Robot = struct { p: Vec2, v: Vec2 };

fn parse(input: []const u8) !std.ArrayList(Robot) {
    var result = std.ArrayList(Robot).init(alloc);
    var blocks = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n");
    while (blocks.next()) |block| {
        var numbers = std.mem.tokenizeAny(u8, block, "pv=, ");
        try result.append(.{
            .p = .{ try std.fmt.parseInt(isize, numbers.next().?, 10), try std.fmt.parseInt(isize, numbers.next().?, 10) },
            .v = .{ try std.fmt.parseInt(isize, numbers.next().?, 10), try std.fmt.parseInt(isize, numbers.next().?, 10) },
        });
    }
    return result;
}
fn part1(input: []const u8, size: Vec2) !usize {
    var robots = try parse(input);
    defer robots.deinit();

    const middle = (size - Vec2{ 1, 1 }) / Vec2{ 2, 2 };
    var quadrants = [_]usize{ 0, 0, 0, 0 };
    for (robots.items) |r| {
        const new_position = @mod(r.p + r.v * Vec2{ 100, 100 }, size);
        if (@reduce(.Or, new_position == middle))
            continue;
        const quadrant_pos = new_position < middle;
        quadrants[@as(usize, if (quadrant_pos[0]) 1 else 0) + @as(usize, if (quadrant_pos[1]) 2 else 0)] += 1;
    }

    return quadrants[0] * quadrants[1] * quadrants[2] * quadrants[3];
}

fn part2(input: []const u8, size: Vec2) !usize {
    var robots = try parse(input);
    defer robots.deinit();

    var framebuffer = std.AutoHashMap(Vec2, void).init(alloc);
    defer framebuffer.deinit();

    var i: usize = 0;
    while (true) : (i += 1) {
        framebuffer.clearRetainingCapacity();
        for (robots.items) |r| {
            try framebuffer.put(@mod(r.p + r.v * Vec2{ @intCast(i), @intCast(i) }, size), {});
        }

        var niceness: usize = 0;
        var pixel_it = framebuffer.keyIterator();
        while (pixel_it.next()) |p| {
            for ([_]Vec2{ .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } }) |d| {
                if (framebuffer.contains(p.* + d))
                    niceness += 1;
            }
        }

        if (niceness < robots.items.len)
            continue;

        for (0..@intCast(size[1])) |y| {
            for (0..@intCast(size[0])) |x| {
                const p = Vec2{ @intCast(x), @intCast(y) };
                try std.io.getStdOut().writer().print("{s}", .{if (framebuffer.contains(p)) "X" else "."});
            }
            try std.io.getStdOut().writer().print("\n", .{});
        }
        return i;
    }
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input14.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input, .{ 101, 103 })});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input, .{ 101, 103 })});
}

test "examples" {
    const input =
        \\p=0,4 v=3,-3
        \\p=6,3 v=-1,-3
        \\p=10,3 v=-1,2
        \\p=2,0 v=2,-1
        \\p=0,0 v=1,3
        \\p=3,0 v=-2,-2
        \\p=7,6 v=-1,-3
        \\p=3,0 v=-1,-2
        \\p=9,3 v=2,3
        \\p=7,3 v=-1,2
        \\p=2,4 v=2,-3
        \\p=9,5 v=-3,-3
        \\
    ;
    try std.testing.expectEqual(12, part1(input, .{ 11, 7 }));
}
