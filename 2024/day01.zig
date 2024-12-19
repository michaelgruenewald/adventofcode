const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

fn parse(input: []const u8) !struct { std.ArrayList(usize), std.ArrayList(usize) } {
    var left = std.ArrayList(usize).init(a);
    var right = std.ArrayList(usize).init(a);

    var lines = std.mem.splitSequence(u8, input, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var pair = std.mem.splitSequence(u8, line, "   ");
        try left.append(try std.fmt.parseInt(usize, pair.next().?, 10));
        try right.append(try std.fmt.parseInt(usize, pair.next().?, 10));
    }

    return .{ left, right };
}

fn part1(input: []const u8) !usize {
    const left, const right = try parse(input);
    defer left.deinit();
    defer right.deinit();

    std.mem.sort(usize, left.items, {}, std.sort.asc(usize));
    std.mem.sort(usize, right.items, {}, std.sort.asc(usize));

    var sum: usize = 0;
    for (left.items, right.items) |l, r| sum += @max(l, r) - @min(l, r);
    return sum;
}

fn part2(input: []const u8) !usize {
    const left, const right = try parse(input);
    defer left.deinit();
    defer right.deinit();

    var right_count = std.AutoHashMap(usize, usize).init(a);
    defer right_count.deinit();
    for (right.items) |r| (try right_count.getOrPutValue(r, 0)).value_ptr.* += 1;

    var sum: usize = 0;
    for (left.items) |l| sum += l * (right_count.get(l) orelse 0);
    return sum;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input01.txt", ~@as(usize, 0));
    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\3   4
        \\4   3
        \\2   5
        \\1   3
        \\3   9
        \\3   3
        \\
    ;
    try std.testing.expectEqual(11, part1(input));
    try std.testing.expectEqual(31, part2(input));
}
