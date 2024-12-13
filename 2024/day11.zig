const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

var known_answers = std.AutoArrayHashMap(struct { usize, usize }, usize).init(a);

fn compute(n: usize, iterations: usize) !usize {
    if (iterations == 0)
        return 1;
    if (known_answers.get(.{ n, iterations })) |r|
        return r;
    var result: usize = undefined;
    if (n == 0)
        result = try compute(1, iterations - 1)
    else {
        const digits = std.math.log10_int(n) + 1;
        if (digits % 2 == 0)
            result = try compute(n / std.math.pow(usize, 10, digits / 2), iterations - 1) +
                try compute(n % std.math.pow(usize, 10, digits / 2), iterations - 1)
        else
            result = try compute(n * 2024, iterations - 1);
    }
    try known_answers.put(.{ n, iterations }, result);
    return result;
}

fn part1(input: []const u8) !usize {
    var sum: usize = 0;
    var it = std.mem.splitScalar(u8, input, ' ');
    while (it.next()) |item|
        sum += try compute(try std.fmt.parseInt(usize, item, 10), 25);
    return sum;
}

fn part2(input: []const u8) !usize {
    var sum: usize = 0;
    var it = std.mem.splitScalar(u8, input, ' ');
    while (it.next()) |item|
        sum += try compute(try std.fmt.parseInt(usize, item, 10), 75);
    return sum;
}

pub fn main() !void {
    const input = std.mem.trim(u8, try std.fs.cwd().readFileAlloc(a, "input11.txt", ~@as(usize, 0)), "\n");

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input = "125 17";
    try std.testing.expectEqual(55312, part1(input));
}
