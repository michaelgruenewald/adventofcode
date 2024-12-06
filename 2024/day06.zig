const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

const Vec2 = @Vector(2, i16);
const Tile = enum { Free, Obstacle };

fn parse(input: []const u8, into: *std.AutoHashMap(Vec2, Tile), start: *Vec2) !void {
    var lines = std.mem.splitSequence(u8, input, "\n");
    var y: usize = 0;
    while (lines.next()) |line| : (y += 1) {
        for (line, 0..) |c, x| {
            const tile: Tile = switch (c) {
                '#' => .Obstacle,
                '^' => start: {
                    start.* = .{ @intCast(x), @intCast(y) };
                    break :start .Free;
                },
                '.' => .Free,
                else => std.debug.panic("Unexpected character '{c}'", .{c}),
            };
            try into.put(.{ @intCast(x), @intCast(y) }, tile);
        }
    }
}

const Direction = enum {
    Up,
    Down,
    Left,
    Right,

    fn apply(self: Direction, point: Vec2) Vec2 {
        return point + switch (self) {
            .Up => Vec2{ 0, -1 },
            .Down => Vec2{ 0, 1 },
            .Left => Vec2{ -1, 0 },
            .Right => Vec2{ 1, 0 },
        };
    }

    fn turnRight(self: Direction) Direction {
        return switch (self) {
            .Up => .Right,
            .Right => .Down,
            .Down => .Left,
            .Left => .Up,
        };
    }
};

const Phase = struct { position: Vec2, direction: Direction };

const Walker = struct {
    map: *const std.AutoHashMap(Vec2, Tile),
    phase: Phase,

    fn next(self: *Walker) ?Phase {
        std.debug.assert(self.map.contains(self.phase.position));
        if (self.map.get(self.phase.direction.apply(self.phase.position)) == .Obstacle) {
            self.phase.direction = self.phase.direction.turnRight();
        } else {
            self.phase.position = self.phase.direction.apply(self.phase.position);
        }
        return if (self.map.contains(self.phase.position)) self.phase else null;
    }
};

fn part1(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, Tile).init(a);
    var start: Vec2 = undefined;
    try parse(input, &map, &start);

    var seen = std.AutoHashMap(Vec2, void).init(a);
    try seen.put(start, {});
    var walker = Walker{ .map = &map, .phase = .{ .position = start, .direction = .Up } };
    while (walker.next()) |phase| {
        try seen.put(phase.position, {});
    }

    return seen.count();
}

fn part2(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, Tile).init(a);
    var start: Vec2 = undefined;
    try parse(input, &map, &start);

    const start_phase = Phase{ .position = start, .direction = .Up };
    var seen = std.AutoHashMap(Phase, void).init(a);
    try seen.put(start_phase, {});
    {
        var walker = Walker{ .map = &map, .phase = start_phase };
        while (walker.next()) |phase|
            try seen.put(phase, {});
    }

    var options = std.AutoHashMap(Vec2, void).init(a);
    var seen_it = seen.keyIterator();
    while (seen_it.next()) |from| {
        const new_obstacle = from.direction.apply(from.position);
        if (std.meta.eql(new_obstacle, start) or map.get(new_obstacle) != .Free)
            continue;

        var modified_map = try map.clone();
        try modified_map.put(new_obstacle, .Obstacle);

        var been = std.AutoHashMap(Phase, void).init(a);
        var walker = Walker{ .map = &modified_map, .phase = start_phase };
        while (walker.next()) |phase| {
            if (been.contains(phase)) {
                try options.put(new_obstacle, {});
                break;
            } else {
                try been.put(phase, {});
            }
        }

        modified_map.deinit();
    }

    return options.count();
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input06.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\....#.....
        \\.........#
        \\..........
        \\..#.......
        \\.......#..
        \\..........
        \\.#..^.....
        \\........#.
        \\#.........
        \\......#...
        \\
    ;
    try std.testing.expectEqual(41, part1(input));
    try std.testing.expectEqual(6, part2(input));
}
