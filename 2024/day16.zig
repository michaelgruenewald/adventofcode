const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, i16);
const State = struct { position: Vec2, direction: Vec2 };
const CostState = struct { cost: usize, state: State };

fn parse(input: []const u8) !struct { std.AutoHashMap(Vec2, void), Vec2, Vec2 } {
    var map = std.AutoHashMap(Vec2, void).init(alloc);
    var start: Vec2 = undefined;
    var end: Vec2 = undefined;

    var lines = std.mem.splitSequence(u8, input, "\n");

    var y: usize = 0;
    while (lines.next()) |line| : (y += 1) {
        for (line, 0..) |c, x| {
            const pos = Vec2{ @intCast(x), @intCast(y) };
            if (c == 'S' or c == 'E' or c == '.')
                try map.put(pos, {});
            if (c == 'S')
                start = pos;
            if (c == 'E')
                end = pos;
        }
    }
    return .{ map, start, end };
}

fn part1(input: []const u8) !usize {
    var map, const start, const end = try parse(input);
    defer map.deinit();

    var work = std.AutoArrayHashMap(State, usize).init(alloc);
    defer work.deinit();
    var finalized = std.AutoHashMap(State, void).init(alloc);
    defer finalized.deinit();

    try work.put(.{ .position = start, .direction = .{ 1, 0 } }, 0);

    while (true) {
        const cheapest = std.sort.argMin(usize, work.values(), {}, std.sort.asc(usize)).?;
        const cost = work.values()[cheapest];
        const state = work.keys()[cheapest];

        if (std.meta.eql(state.position, end)) {
            return cost;
        }

        _ = work.swapRemove(state);
        try finalized.put(state, {});

        for ([_]struct { State, usize }{
            .{ .{ .position = state.position + state.direction, .direction = state.direction }, cost + 1 },
            .{ .{ .position = state.position, .direction = .{ state.direction[1], -state.direction[0] } }, cost + 1000 },
            .{ .{ .position = state.position, .direction = .{ -state.direction[1], state.direction[0] } }, cost + 1000 },
        }) |i| {
            const new_state, const new_cost = i;
            if (finalized.contains(new_state) or !map.contains(new_state.position))
                continue;
            const e = try work.getOrPut(new_state);
            if (!e.found_existing or e.value_ptr.* > new_cost) {
                e.value_ptr.* = new_cost;
            }
        }
    }
}

const CostPath = struct {
    cost: usize,
    path: std.AutoHashMap(Vec2, void),
};

fn cheaper(_: void, lhs: CostPath, rhs: CostPath) bool {
    return lhs.cost < rhs.cost;
}

fn part2(input: []const u8) !usize {
    var map, const start, const end = try parse(input);
    defer map.deinit();

    var work = std.AutoArrayHashMap(State, CostPath).init(alloc);
    defer work.deinit();
    var finalized = std.AutoHashMap(State, void).init(alloc);
    defer finalized.deinit();

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    try work.put(
        .{ .position = start, .direction = .{ 1, 0 } },
        .{ .cost = 0, .path = std.AutoHashMap(Vec2, void).init(arena_alloc) },
    );

    const result = while (true) {
        const cheapest = std.sort.argMin(CostPath, work.values(), {}, cheaper).?;
        const cost_path = work.values()[cheapest];
        const state = work.keys()[cheapest];

        if (std.meta.eql(state.position, end)) {
            break cost_path;
        }

        _ = work.swapRemove(state);
        try finalized.put(state, {});

        for ([_]struct { State, usize }{
            .{ .{ .position = state.position + state.direction, .direction = state.direction }, cost_path.cost + 1 },
            .{ .{ .position = state.position, .direction = .{ state.direction[1], -state.direction[0] } }, cost_path.cost + 1000 },
            .{ .{ .position = state.position, .direction = .{ -state.direction[1], state.direction[0] } }, cost_path.cost + 1000 },
        }) |i| {
            const new_state, const new_cost = i;
            if (finalized.contains(new_state) or !map.contains(new_state.position))
                continue;
            const e = try work.getOrPut(new_state);
            if (!e.found_existing or e.value_ptr.*.cost > new_cost) {
                e.value_ptr.*.cost = new_cost;
                var new_path = try cost_path.path.clone();
                try new_path.put(state.position, {});
                e.value_ptr.*.path = new_path;
            } else if (e.value_ptr.*.cost == new_cost) {
                var path_it = cost_path.path.keyIterator();
                while (path_it.next()) |p| try e.value_ptr.*.path.put(p.*, {});
                try e.value_ptr.*.path.put(state.position, {});
            }
        }
    };

    return result.path.count() + 1;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input16.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input1 =
        \\###############
        \\#.......#....E#
        \\#.#.###.#.###.#
        \\#.....#.#...#.#
        \\#.###.#####.#.#
        \\#.#.#.......#.#
        \\#.#.#####.###.#
        \\#...........#.#
        \\###.#.#####.#.#
        \\#...#.....#.#.#
        \\#.#.#.###.#.#.#
        \\#.....#...#.#.#
        \\#.###.#.#.#.#.#
        \\#S..#.....#...#
        \\###############
        \\
    ;
    try std.testing.expectEqual(7036, part1(input1));
    try std.testing.expectEqual(45, part2(input1));

    const input2 =
        \\#################
        \\#...#...#...#..E#
        \\#.#.#.#.#.#.#.#.#
        \\#.#.#.#...#...#.#
        \\#.#.#.#.###.#.#.#
        \\#...#.#.#.....#.#
        \\#.#.#.#.#.#####.#
        \\#.#...#.#.#.....#
        \\#.#.#####.#.###.#
        \\#.#.#.......#...#
        \\#.#.###.#####.###
        \\#.#.#...#.....#.#
        \\#.#.#.#####.###.#
        \\#.#.#.........#.#
        \\#.#.#.#########.#
        \\#S#.............#
        \\#################
        \\
    ;
    try std.testing.expectEqual(11048, part1(input2));
    try std.testing.expectEqual(64, part2(input2));
}
