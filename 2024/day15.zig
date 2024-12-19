const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, i16);
const Tile = enum { Free, Box, Obstacle };
const Tile2 = enum { Free, BoxL, BoxR, Obstacle };

fn parse(input: []const u8) !struct { std.AutoHashMap(Vec2, Tile), std.ArrayList(Vec2), Vec2 } {
    var blocks = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n\n");

    var lines = std.mem.splitSequence(u8, blocks.next().?, "\n");
    var y: usize = 0;

    var robot: Vec2 = undefined;
    var map = std.AutoHashMap(Vec2, Tile).init(alloc);
    while (lines.next()) |line| : (y += 1) {
        for (line, 0..) |c, x| {
            try map.put(
                .{ @intCast(x), @intCast(y) },
                switch (c) {
                    '@' => at: {
                        robot = .{ @intCast(x), @intCast(y) };
                        break :at .Free;
                    },
                    '.' => .Free,
                    '#' => .Obstacle,
                    'O' => .Box,
                    else => unreachable,
                },
            );
        }
    }

    var moves = std.ArrayList(Vec2).init(alloc);
    for (blocks.next().?) |c| {
        const move = switch (c) {
            '<' => Vec2{ -1, 0 },
            '>' => Vec2{ 1, 0 },
            '^' => Vec2{ 0, -1 },
            'v' => Vec2{ 0, 1 },
            else => null,
        };
        if (move) |m| try moves.append(m);
    }

    return .{ map, moves, robot };
}

fn tryMove(map: *std.AutoHashMap(Vec2, Tile), position: Vec2, direction: Vec2) bool {
    const self = map.getEntry(position).?;
    const target = map.getEntry(position + direction).?;
    switch (target.value_ptr.*) {
        .Free => {},
        .Box => if (!tryMove(map, position + direction, direction))
            return false,
        .Obstacle => return false,
    }

    std.mem.swap(Tile, self.value_ptr, target.value_ptr);
    return true;
}

fn part1(input: []const u8) !usize {
    var map, const moves, var robot = try parse(input);
    defer map.deinit();
    defer moves.deinit();

    for (moves.items) |m| {
        if (tryMove(&map, robot, m))
            robot += m;
    }

    var sum: usize = 0;
    var map_it = map.iterator();
    while (map_it.next()) |e| {
        if (e.value_ptr.* == .Box)
            sum += @as(usize, @intCast(e.key_ptr.*[1] * 100)) + @as(usize, @intCast(e.key_ptr.*[0]));
    }

    return sum;
}

fn canMove(map: *std.AutoHashMap(Vec2, Tile2), position: Vec2, direction: Vec2) bool {
    const target_pos = position + direction;
    const vertical = direction[0] == 0;
    return switch (map.get(position).?) {
        .Free => true,
        .Obstacle => false,
        .BoxL => canMove(map, target_pos, direction) and
            if (vertical) canMove(map, target_pos + Vec2{ 1, 0 }, direction) else true,
        .BoxR => canMove(map, target_pos, direction) and
            if (vertical) canMove(map, target_pos - Vec2{ 1, 0 }, direction) else true,
    };
}

fn doMove(map: *std.AutoHashMap(Vec2, Tile2), position: Vec2, direction: Vec2) void {
    const self = map.get(position).?;
    if (self == .Free)
        return;

    const target_pos = position + direction;
    doMove(map, target_pos, direction);
    map.getEntry(target_pos).?.value_ptr.* = self;
    map.getEntry(position).?.value_ptr.* = .Free;

    const vertical = direction[0] == 0;
    if (vertical and self == .BoxL) {
        doMove(map, position + Vec2{ 1, 0 }, direction);
    } else if (vertical and self == .BoxR) {
        doMove(map, position - Vec2{ 1, 0 }, direction);
    }
}

fn part2(input: []const u8) !usize {
    var original_map, const moves, var robot = try parse(input);
    defer original_map.deinit();
    defer moves.deinit();

    var map = std.AutoHashMap(Vec2, Tile2).init(alloc);
    defer map.deinit();
    var original_map_it = original_map.iterator();
    while (original_map_it.next()) |e| {
        try map.put(e.key_ptr.* * Vec2{ 2, 1 }, switch (e.value_ptr.*) {
            .Free => .Free,
            .Obstacle => .Obstacle,
            .Box => .BoxL,
        });
        try map.put(e.key_ptr.* * Vec2{ 2, 1 } + Vec2{ 1, 0 }, switch (e.value_ptr.*) {
            .Free => .Free,
            .Obstacle => .Obstacle,
            .Box => .BoxR,
        });
    }
    robot *= Vec2{ 2, 1 };

    for (moves.items) |m| {
        const target = robot + m;
        if (canMove(&map, target, m)) {
            doMove(&map, target, m);
            robot = target;
        }
    }

    var sum: usize = 0;
    var map_it = map.iterator();
    while (map_it.next()) |e| {
        if (e.value_ptr.* == .BoxL)
            sum += @as(usize, @intCast(e.key_ptr.*[1] * 100)) + @as(usize, @intCast(e.key_ptr.*[0]));
    }

    return sum;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input15.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "smaller examples" {
    const input =
        \\########
        \\#..O.O.#
        \\##@.O..#
        \\#...O..#
        \\#.#.O..#
        \\#...O..#
        \\#......#
        \\########
        \\
        \\<^^>>>vv<v>>v<<
        \\
    ;
    try std.testing.expectEqual(2028, part1(input));
}

test "examples" {
    const input =
        \\##########
        \\#..O..O.O#
        \\#......O.#
        \\#.OO..O.O#
        \\#..O@..O.#
        \\#O#..O...#
        \\#O..O..O.#
        \\#.OO.O.OO#
        \\#....O...#
        \\##########
        \\
        \\<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
        \\vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
        \\><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
        \\<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
        \\^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
        \\^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
        \\>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
        \\<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
        \\^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
        \\v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
        \\
    ;
    try std.testing.expectEqual(10092, part1(input));
    try std.testing.expectEqual(9021, part2(input));
}
