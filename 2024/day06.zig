const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

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

const Phase = struct {
    position: Vec2,
    direction: Direction,

    fn ahead(self: Phase) Vec2 {
        return self.direction.apply(self.position);
    }
};

const Walker = struct {
    map: *const std.AutoHashMap(Vec2, Tile),
    phase: Phase,

    extra_obstacle: ?Vec2 = null,

    fn next(self: *Walker) ?Phase {
        if ((self.extra_obstacle != null and std.meta.eql(self.extra_obstacle.?, self.phase.ahead())) or self.map.get(self.phase.ahead()) == .Obstacle) {
            self.phase.direction = self.phase.direction.turnRight();
        } else {
            self.phase.position = self.phase.ahead();
        }
        return if (self.map.contains(self.phase.position)) self.phase else null;
    }
};

fn part1(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, Tile).init(a);
    defer map.deinit();
    var start: Vec2 = undefined;
    try parse(input, &map, &start);

    var seen = std.AutoHashMap(Vec2, void).init(a);
    defer seen.deinit();
    try seen.put(start, {});
    var walker = Walker{ .map = &map, .phase = .{ .position = start, .direction = .Up } };
    while (walker.next()) |phase| {
        try seen.put(phase.position, {});
    }

    return seen.count();
}

fn part2(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, Tile).init(a);
    defer map.deinit();
    var start: Vec2 = undefined;
    try parse(input, &map, &start);

    var tried = std.AutoHashMap(Vec2, void).init(a);
    defer tried.deinit();
    try tried.put(start, {});
    var options = std.AutoHashMap(Vec2, void).init(a);
    defer options.deinit();
    var walker = Walker{ .map = &map, .phase = .{ .position = start, .direction = .Up } };
    while (walker.next()) |phase| {
        if (tried.contains(phase.ahead()))
            continue;
        try tried.put(phase.ahead(), {});

        var been = std.AutoHashMap(Phase, void).init(a);
        defer been.deinit();

        try been.put(phase, {});
        var test_walker = Walker{ .map = &map, .phase = phase, .extra_obstacle = phase.ahead() };
        while (test_walker.next()) |p| {
            if (been.contains(p)) {
                try options.put(phase.ahead(), {});
                break;
            } else {
                try been.put(p, {});
            }
        }
    }

    return options.count();
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input06.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
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
