const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

fn parse(input: []const u8, into: *std.ArrayList(std.ArrayList(isize))) !void {
    var lines = std.mem.splitSequence(u8, input, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var row = try into.addOne();
        row.* = std.ArrayList(isize).init(a);
        var splitted = std.mem.splitScalar(u8, line, ' ');
        while (splitted.next()) |c| try row.append(try std.fmt.parseInt(isize, c, 10));
    }
}

fn part1(input: []const u8) !usize {
    var rows = std.ArrayList(std.ArrayList(isize)).init(a);
    defer rows.deinit();
    defer for (rows.items) |row| row.deinit();
    try parse(input, &rows);

    var total: usize = 0;
    for (rows.items) |row| {
        const first = row.items[1] - row.items[0];
        var safe = true;
        for (row.items[0 .. row.items.len - 1], row.items[1..]) |x, y| {
            if (@abs(y - x) < 1 or @abs(y - x) > 3 or (y - x) * first < 0) {
                safe = false;
                break;
            }
        }
        if (safe) {
            total += 1;
        }
    }
    return total;
}

inline fn remap(i: usize, drop: usize) usize {
    return if (i < drop) i else i + 1;
}

fn part2(input: []const u8) !usize {
    var rows = std.ArrayList(std.ArrayList(isize)).init(a);
    defer rows.deinit();
    defer for (rows.items) |row| row.deinit();
    try parse(input, &rows);

    var total: usize = 0;
    for (rows.items) |row| {
        dropping: for (0..row.items.len) |drop| {
            const first = row.items[remap(1, drop)] - row.items[remap(0, drop)];
            var safe = true;
            for (0..row.items.len - 2) |i| {
                const x = row.items[remap(i, drop)];
                const y = row.items[remap(i + 1, drop)];
                if (@abs(y - x) < 1 or @abs(y - x) > 3 or (y - x) * first < 0) {
                    safe = false;
                    break;
                }
            }
            if (safe) {
                total += 1;
                break :dropping;
            }
        }
    }
    return total;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input02.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\7 6 4 2 1
        \\1 2 7 8 9
        \\9 7 6 2 1
        \\1 3 2 4 5
        \\8 6 4 4 1
        \\1 3 6 7 9
        \\
    ;
    try std.testing.expectEqual(2, part1(input));
    try std.testing.expectEqual(4, part2(input));
}
