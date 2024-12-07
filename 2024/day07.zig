const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

fn check(target: u64, so_far: u64, rest: []u64) bool {
    if (rest.len == 0)
        return target == so_far;
    return check(target, so_far + rest[0], rest[1..]) or check(target, so_far * rest[0], rest[1..]);
}

fn part1(input: []const u8) !usize {
    var good: usize = 0;
    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        var it = std.mem.tokenizeAny(u8, line, ": ");
        const target = try std.fmt.parseInt(u64, it.next().?, 10);
        var l = std.ArrayList(u64).init(a);
        defer l.deinit();
        while (it.next()) |s|
            try l.append(try std.fmt.parseInt(u64, s, 10));

        if (check(target, 0, l.items))
            good += target;
    }

    return good;
}

fn check2(target: u64, so_far: u64, rest: []u64) bool {
    if (rest.len == 0)
        return target == so_far;
    return check2(target, so_far + rest[0], rest[1..]) or
        check2(target, so_far * rest[0], rest[1..]) or
        check2(target, so_far * std.math.pow(u64, 10, std.math.log10_int(rest[0]) + 1) + rest[0], rest[1..]);
}

fn part2(input: []const u8) !usize {
    var good: usize = 0;
    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        var it = std.mem.tokenizeAny(u8, line, ": ");
        const target = try std.fmt.parseInt(u64, it.next().?, 10);
        var l = std.ArrayList(u64).init(a);
        defer l.deinit();
        while (it.next()) |s|
            try l.append(try std.fmt.parseInt(u64, s, 10));

        if (check2(target, 0, l.items))
            good += target;
    }

    return good;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input07.txt", ~@as(usize, 0));
    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\190: 10 19
        \\3267: 81 40 27
        \\83: 17 5
        \\156: 15 6
        \\7290: 6 8 6 15
        \\161011: 16 10 13
        \\192: 17 8 14
        \\21037: 9 7 18 13
        \\292: 11 6 16 20
        \\
    ;
    try std.testing.expectEqual(3749, part1(input));
    try std.testing.expectEqual(11387, part2(input));
}
