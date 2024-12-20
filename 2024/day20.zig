const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, i16);

fn parse(input: []const u8) !struct { std.AutoHashMap(Vec2, void), Vec2, Vec2 } {
    var map = std.AutoHashMap(Vec2, void).init(alloc);
    var start: Vec2 = undefined;
    var end: Vec2 = undefined;

    var lines = std.mem.splitSequence(u8, input, "\n");

    var y: usize = 1;
    while (lines.next()) |line| : (y += 1) {
        for (line, 1..) |c, x| {
            const pos = Vec2{ @intCast(x), @intCast(y) };
            if (c != '#') try map.put(pos, {});
            if (c == 'S') start = pos;
            if (c == 'E') end = pos;
        }
    }
    return .{ map, start, end };
}

fn run(input: []const u8, min_saving: usize, max_distance: usize) !usize {
    var map, const start, const end = try parse(input);
    defer map.deinit();

    var path = std.ArrayList(Vec2).init(alloc);
    defer path.deinit();

    var previous_pos: ?Vec2 = null;
    var pos = start;
    try path.append(pos);
    while (!std.meta.eql(pos, end)) {
        for ([_]Vec2{ .{ 0, -1 }, .{ 0, 1 }, .{ -1, 0 }, .{ 1, 0 } }) |d| {
            if (map.contains(pos + d) and !std.meta.eql(previous_pos, (pos + d))) {
                previous_pos = pos;
                pos = pos + d;
                try path.append(pos);
                break;
            }
        } else std.debug.panic("this shall not happen", .{});
    }

    var cheats: usize = 0;
    for (0..path.items.len) |i| {
        const from = path.items[i];
        for (i + 1..path.items.len) |j| {
            const to = path.items[j];
            const distance = @reduce(.Add, @abs(from - to));
            const leap = j - i;
            if (distance <= max_distance and leap >= min_saving + distance)
                cheats += 1;
        }
    }
    return cheats;
}

fn part1(input: []const u8, min_saving: usize) !usize {
    return run(input, min_saving, 2);
}

fn part2(input: []const u8, min_saving: usize) !usize {
    return run(input, min_saving, 20);
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input20.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input, 100)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input, 100)});
}

test "examples" {
    const input =
        \\###############
        \\#...#...#.....#
        \\#.#.#.#.#.###.#
        \\#S#...#.#.#...#
        \\#######.#.#.###
        \\#######.#.#...#
        \\#######.#.###.#
        \\###..E#...#...#
        \\###.#######.###
        \\#...###...#...#
        \\#.#####.#.###.#
        \\#.#...#.#.#...#
        \\#.#.#.#.#.#.###
        \\#...#...#...###
        \\###############
        \\
    ;
    try std.testing.expectEqual(5, try part1(input, 20));
    try std.testing.expectEqual(285, try part2(input, 50));
}
