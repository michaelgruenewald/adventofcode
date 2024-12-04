const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

const Point = @Vector(2, i16);

fn parse(input: []const u8, into: *std.AutoHashMap(Point, u8)) !void {
    var lines = std.mem.splitSequence(u8, input, "\n");
    var y: usize = 0;
    while (lines.next()) |line| : (y += 1) {
        for (line, 0..) |c, x| {
            try into.put(.{ @intCast(x), @intCast(y) }, c);
        }
    }
}

fn part1(input: []const u8) !usize {
    const DIRECTIONS = [_]Point{ .{ 1, 0 }, .{ -1, 0 }, .{ 0, 1 }, .{ 0, -1 }, .{ 1, 1 }, .{ -1, 1 }, .{ -1, -1 }, .{ 1, -1 } };

    var map = std.AutoHashMap(Point, u8).init(a);
    try parse(input, &map);

    var total: usize = 0;
    var y: i16 = 0;
    while (map.contains(.{ 0, y })) : (y += 1) {
        var x: i16 = 0;
        while (map.contains(.{ x, y })) : (x += 1) {
            directions: for (DIRECTIONS) |d| {
                for ("XMAS", 0..) |c, i| {
                    if (map.get(Point{ x, y } + d * @as(Point, @splat(@intCast(i)))) != c) {
                        continue :directions;
                    }
                }
                total += 1;
            }
        }
    }

    return total;
}

const Rotation = enum {
    none,
    once,
    twice,
    thrice,

    fn apply(self: Rotation, point: Point) Point {
        return switch (self) {
            Rotation.none => .{ point[0], point[1] },
            Rotation.once => .{ point[1], -point[0] },
            Rotation.twice => .{ -point[0], -point[1] },
            Rotation.thrice => .{ -point[1], point[0] },
        };
    }
};

fn part2(input: []const u8) !usize {
    const XMAS = [_]struct { what: u8, where: Point }{
        .{ .what = 'A', .where = .{ 0, 0 } },
        .{ .what = 'M', .where = .{ -1, -1 } },
        .{ .what = 'M', .where = .{ -1, 1 } },
        .{ .what = 'S', .where = .{ 1, -1 } },
        .{ .what = 'S', .where = .{ 1, 1 } },
    };

    var map = std.AutoHashMap(Point, u8).init(a);
    try parse(input, &map);

    var total: usize = 0;
    var y: i16 = 0;
    while (map.contains(.{ 0, y })) : (y += 1) {
        var x: i16 = 0;
        while (map.contains(.{ x, y })) : (x += 1) {
            rotations: for ([_]Rotation{ .none, .once, .twice, .thrice }) |r| {
                for (XMAS) |symbol| {
                    const rotated = r.apply(symbol.where);
                    if (map.get(Point{ x, y } + rotated) != symbol.what) {
                        continue :rotations;
                    }
                }
                total += 1;
            }
        }
    }

    return total;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input04.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\MMMSXXMASM
        \\MSAMXMSMSA
        \\AMXSXMAAMM
        \\MSAMASMSMX
        \\XMASAMXAMM
        \\XXAMMXXAMA
        \\SMSMSASXSS
        \\SAXAMASAAA
        \\MAMMMXMMMM
        \\MXMXAXMASX
        \\
    ;
    try std.testing.expectEqual(18, part1(input));
    try std.testing.expectEqual(9, part2(input));
}
