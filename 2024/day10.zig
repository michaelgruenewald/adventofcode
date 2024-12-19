const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, i16);

fn part1(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, usize).init(a);
    defer map.deinit();

    var lines = std.mem.splitSequence(u8, input, "\n");
    var y: usize = 0;
    while (lines.next()) |line| : (y += 1) {
        for (line, 0..) |c, x| {
            try map.put(.{ @intCast(x), @intCast(y) }, try std.fmt.charToDigit(c, 10));
        }
    }

    var sum: usize = 0;
    var head_it = map.iterator();
    while (head_it.next()) |head_item| {
        if (head_item.value_ptr.* != 0) continue;

        var look_at = std.AutoHashMap(Vec2, void).init(a);
        defer look_at.deinit();
        try look_at.put(head_item.key_ptr.*, {});
        for (1..10) |next_level| {
            var look_at_next = std.AutoHashMap(Vec2, void).init(a);
            var look_at_it = look_at.iterator();
            while (look_at_it.next()) |p| {
                for ([_]Vec2{ .{ 0, -1 }, .{ 0, 1 }, .{ -1, 0 }, .{ 1, 0 } }) |d| {
                    if (map.get(p.key_ptr.* + d) == next_level) {
                        try look_at_next.put(p.key_ptr.* + d, {});
                    }
                }
            }
            look_at.deinit();
            look_at = look_at_next;
        }
        sum += look_at.count();
    }

    return sum;
}

fn part2(input: []const u8) !usize {
    var map = std.AutoHashMap(Vec2, usize).init(a);
    defer map.deinit();

    var lines = std.mem.splitSequence(u8, input, "\n");
    var y: usize = 0;
    while (lines.next()) |line| : (y += 1) {
        for (line, 0..) |c, x| {
            try map.put(.{ @intCast(x), @intCast(y) }, try std.fmt.charToDigit(c, 10));
        }
    }

    var sum: usize = 0;
    var head_it = map.iterator();
    while (head_it.next()) |head_item| {
        if (head_item.value_ptr.* != 0) continue;

        var look_at = std.ArrayList(Vec2).init(a);
        defer look_at.deinit();
        try look_at.append(head_item.key_ptr.*);
        for (1..10) |next_level| {
            var look_at_next = std.ArrayList(Vec2).init(a);
            for (look_at.items) |p| {
                for ([_]Vec2{ .{ 0, -1 }, .{ 0, 1 }, .{ -1, 0 }, .{ 1, 0 } }) |d| {
                    if (map.get(p + d) == next_level) {
                        try look_at_next.append(p + d);
                    }
                }
            }
            look_at.deinit();
            look_at = look_at_next;
        }
        sum += look_at.items.len;
    }

    return sum;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input10.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\89010123
        \\78121874
        \\87430965
        \\96549874
        \\45678903
        \\32019012
        \\01329801
        \\10456732
        \\
    ;
    try std.testing.expectEqual(36, part1(input));
    try std.testing.expectEqual(81, part2(input));
}
