const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, i16);

fn numeric_pad_pos(c: u8) Vec2 {
    return switch (c) {
        '7' => .{ 1, 1 },
        '8' => .{ 2, 1 },
        '9' => .{ 3, 1 },
        '4' => .{ 1, 2 },
        '5' => .{ 2, 2 },
        '6' => .{ 3, 2 },
        '1' => .{ 1, 3 },
        '2' => .{ 2, 3 },
        '3' => .{ 3, 3 },
        '0' => .{ 2, 4 },
        'A' => .{ 3, 4 },
        else => std.debug.panic("What is {c}??", .{c}),
    };
}

fn directional_pad_pos(c: u8) Vec2 {
    return switch (c) {
        '^' => .{ 2, 1 },
        'A' => .{ 3, 1 },
        '<' => .{ 1, 2 },
        'v' => .{ 2, 2 },
        '>' => .{ 3, 2 },
        else => std.debug.panic("What is {c}??", .{c}),
    };
}

fn compute(code: []const u8, level: usize, max_level: usize, cache: *std.AutoHashMap(usize, std.StringHashMap(usize))) !usize {
    if (cache.*.get(level)) |level_cache| if (level_cache.get(code)) |r| return r;

    const matrix = if (level == 0) &numeric_pad_pos else &directional_pad_pos;
    var pos = matrix('A');
    var total: usize = 0;

    for (code) |c| {
        const new_pos = matrix(c);

        if (level == max_level) {
            total += @reduce(.Add, @abs(new_pos - pos)) + 1;
        } else {
            var best: ?usize = null;
            for ([_]bool{ true, false }) |v_before_h| {
                var presses = std.ArrayList(u8).init(alloc);
                defer presses.deinit();

                const d = new_pos - pos;
                if (v_before_h) {
                    if ((level == 0 and pos[0] == 1 and new_pos[1] == 4) or
                        (level > 0 and pos[0] == 1 and new_pos[1] == 1))
                        continue; // illegal!
                    try presses.appendNTimes(if (d[1] < 0) '^' else 'v', @abs(d[1]));
                    try presses.appendNTimes(if (d[0] < 0) '<' else '>', @abs(d[0]));
                } else {
                    if ((level == 0 and new_pos[0] == 1 and pos[1] == 4) or
                        (level > 0 and new_pos[0] == 1 and pos[1] == 1))
                        continue; // illegal!
                    try presses.appendNTimes(if (d[0] < 0) '<' else '>', @abs(d[0]));
                    try presses.appendNTimes(if (d[1] < 0) '^' else 'v', @abs(d[1]));
                }
                try presses.append('A');

                const this = try compute(presses.items, level + 1, max_level, cache);
                if (best == null or best.? > this)
                    best = this;
            }
            total += best.?;
        }

        pos = new_pos;
    }

    const level_cache = try cache.getOrPut(level);
    if (!level_cache.found_existing)
        level_cache.value_ptr.* = std.StringHashMap(usize).init(cache.allocator);
    try level_cache.value_ptr.*.put(try cache.allocator.dupe(u8, code), total);

    return total;
}

fn part1(input: []const u8) !usize {
    var result: usize = 0;
    var lines = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n");
    var cache_arena = std.heap.ArenaAllocator.init(alloc);
    defer cache_arena.deinit();
    var cache = std.AutoHashMap(usize, std.StringHashMap(usize)).init(cache_arena.allocator());
    while (lines.next()) |code| {
        const numeric = try std.fmt.parseInt(usize, code[0 .. code.len - 1], 10);
        const moves = try compute(code, 0, 2, &cache);
        result += numeric * moves;
    }
    return result;
}

fn part2(input: []const u8) !usize {
    var result: usize = 0;
    var lines = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n");
    var cache_arena = std.heap.ArenaAllocator.init(alloc);
    defer cache_arena.deinit();
    var cache = std.AutoHashMap(usize, std.StringHashMap(usize)).init(cache_arena.allocator());
    while (lines.next()) |code| {
        const numeric = try std.fmt.parseInt(usize, code[0 .. code.len - 1], 10);
        const moves = try compute(code, 0, 25, &cache);
        result += numeric * moves;
    }
    return result;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input21.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\029A
        \\980A
        \\179A
        \\456A
        \\379A
        \\
    ;
    try std.testing.expectEqual(126384, try part1(input));
}
