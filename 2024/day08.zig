const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

const Vec2 = @Vector(2, i16);
const Tile = union((enum { Free, Antenna })) { Free, Antenna: u8 };

fn parse(input: []const u8, into: *std.AutoHashMap(Vec2, Tile)) !void {
    var lines = std.mem.splitSequence(u8, input, "\n");
    var y: usize = 0;
    while (lines.next()) |line| : (y += 1)
        for (line, 0..) |c, x|
            try into.put(
                .{ @intCast(x), @intCast(y) },
                if (c == '.') .{ .Free = {} } else .{ .Antenna = c },
            );
}

fn group_by_freq(map: *const std.AutoHashMap(Vec2, Tile)) !std.AutoHashMap(u8, std.ArrayList(Vec2)) {
    var by_freq = std.AutoHashMap(u8, std.ArrayList(Vec2)).init(a);
    var map_iter = map.iterator();
    while (map_iter.next()) |e| {
        switch (e.value_ptr.*) {
            .Antenna => |f| {
                const slot = try by_freq.getOrPut(f);
                if (!slot.found_existing) slot.value_ptr.* = std.ArrayList(Vec2).init(a);
                try slot.value_ptr.append(e.key_ptr.*);
            },
            .Free => {},
        }
    }
    return by_freq;
}

fn part1(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, Tile).init(a);
    try parse(input, &map);
    var by_freq = try group_by_freq(&map);
    var antinodes = std.AutoHashMap(Vec2, void).init(a);
    var freq_iter = by_freq.iterator();
    while (freq_iter.next()) |e| {
        for (e.value_ptr.items) |x| {
            for (e.value_ptr.items) |y| {
                if (@reduce(.And, x == y)) continue;
                for ([_]Vec2{ x + x - y, y + y - x }) |v|
                    if (map.contains(v))
                        try antinodes.put(v, {});
            }
        }
    }

    return antinodes.count();
}

fn part2(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, Tile).init(a);
    try parse(input, &map);
    var by_freq = try group_by_freq(&map);
    var antinodes = std.AutoHashMap(Vec2, void).init(a);
    var freq_iter = by_freq.iterator();
    while (freq_iter.next()) |e| {
        for (e.value_ptr.items) |p| {
            for (e.value_ptr.items) |q| {
                if (@reduce(.And, p == q)) continue;
                var s = p;
                while (map.contains(s)) : (s -= q - p)
                    if (map.contains(s))
                        try antinodes.put(s, {});
                s = p;
                while (map.contains(s)) : (s += q - p)
                    if (map.contains(s))
                        try antinodes.put(s, {});
            }
        }
    }

    return antinodes.count();
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input08.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\............
        \\........0...
        \\.....0......
        \\.......0....
        \\....0.......
        \\......A.....
        \\............
        \\............
        \\........A...
        \\.........A..
        \\............
        \\............
        \\
    ;
    try std.testing.expectEqual(14, part1(input));
    try std.testing.expectEqual(34, part2(input));
}
