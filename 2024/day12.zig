const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, i16);
const DIRS = [_]Vec2{ .{ 0, -1 }, .{ 0, 1 }, .{ -1, 0 }, .{ 1, 0 } };

fn parse(input: []const u8, into: *std.AutoHashMap(Vec2, u8)) !void {
    var lines = std.mem.splitSequence(u8, input, "\n");
    var y: usize = 0;
    while (lines.next()) |line| : (y += 1)
        for (line, 0..) |c, x|
            try into.put(.{ @intCast(x), @intCast(y) }, c);
}

fn any(comptime K: type, comptime V: type, map: *const std.AutoHashMap(K, V)) ?std.AutoHashMap(K, V).Entry {
    var it = map.iterator();
    return it.next();
}

fn part1(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, u8).init(a);
    defer map.deinit();
    try parse(input, &map);

    var sum: usize = 0;
    while (any(Vec2, u8, &map)) |start| {
        const plant = start.value_ptr.*;

        var work = std.ArrayList(Vec2).init(a);
        defer work.deinit();
        var found = std.AutoHashMap(Vec2, void).init(a);
        defer found.deinit();

        try work.append(start.key_ptr.*);
        while (work.popOrNull()) |look_at| {
            if (!map.remove(look_at))
                continue;
            try found.put(look_at, {});
            for (DIRS) |d| {
                if (map.get(look_at + d) == plant) {
                    try work.append(look_at + d);
                }
            }
        }
        const area = found.count();
        var perimeter: usize = 0;
        var it = found.keyIterator();
        while (it.next()) |f| {
            for (DIRS) |d| {
                if (!found.contains(f.* + d))
                    perimeter += 1;
            }
        }

        sum += area * perimeter;
    }

    return sum;
}

fn part2(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, u8).init(a);
    defer map.deinit();
    try parse(input, &map);

    var sum: usize = 0;
    while (any(Vec2, u8, &map)) |start| {
        const plant = start.value_ptr.*;

        var work = std.ArrayList(Vec2).init(a);
        defer work.deinit();
        var found = std.AutoHashMap(Vec2, void).init(a);
        defer found.deinit();

        try work.append(start.key_ptr.*);
        while (work.popOrNull()) |look_at| {
            if (!map.remove(look_at))
                continue;
            try found.put(look_at, {});
            for (DIRS) |d| {
                if (map.get(look_at + d) == plant)
                    try work.append(look_at + d);
            }
        }

        const area = found.count();

        var left_down_edge_nodes = std.ArrayList(Vec2).init(a);
        defer left_down_edge_nodes.deinit();
        var found_it = found.keyIterator();
        while (found_it.next()) |n| {
            if (!found.contains(n.* - Vec2{ 1, 0 }))
                try left_down_edge_nodes.append(n.*);
        }

        var sides: usize = 0;
        for (left_down_edge_nodes.items) |lden| {
            var inner_position = lden;
            var outer_position = lden - Vec2{ 1, 0 };
            var direction = Vec2{ 0, 1 };

            while (true) {
                const i = found.contains(inner_position + direction);
                const o = found.contains(outer_position + direction);
                if (i and !o) {
                    inner_position += direction;
                    outer_position += direction;
                } else if (i and o) {
                    // turn right
                    inner_position = outer_position + direction;
                    direction = Vec2{ -direction[1], direction[0] };
                    sides += 1;
                } else if (!i) {
                    // turn left
                    outer_position = inner_position + direction;
                    direction = Vec2{ direction[1], -direction[0] };
                    sides += 1;
                } else unreachable;
                if (@reduce(.And, direction == Vec2{ 0, 1 }))
                    break;
            }
        }

        sum += area * sides;
    }

    return sum;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input12.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "small examples" {
    try std.testing.expectEqual(772, part1(
        \\OOOOO
        \\OXOXO
        \\OOOOO
        \\OXOXO
        \\OOOOO
    ));
    try std.testing.expectEqual(80, part2(
        \\AAAA
        \\BBCD
        \\BBCC
        \\EEEC
    ));
    try std.testing.expectEqual(436, part2(
        \\OOOOO
        \\OXOXO
        \\OOOOO
        \\OXOXO
        \\OOOOO
    ));
    try std.testing.expectEqual(236, part2(
        \\EEEEE
        \\EXXXX
        \\EEEEE
        \\EXXXX
        \\EEEEE
    ));
    try std.testing.expectEqual(368, part2(
        \\AAAAAA
        \\AAABBA
        \\AAABBA
        \\ABBAAA
        \\ABBAAA
        \\AAAAAA
    ));
}

test "examples" {
    const input =
        \\RRRRIICCFF
        \\RRRRIICCCF
        \\VVRRRCCFFF
        \\VVRCCCJFFF
        \\VVVVCJJCFE
        \\VVIVCCJJEE
        \\VVIIICJJEE
        \\MIIIIIJJEE
        \\MIIISIJEEE
        \\MMMISSJEEE
        \\
    ;
    try std.testing.expectEqual(1930, part1(input));
    try std.testing.expectEqual(1206, part2(input));
}
