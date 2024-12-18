const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, isize);
const Machine = struct { a: Vec2, b: Vec2, p: Vec2 };

fn parse(input: []const u8) !std.ArrayList(Machine) {
    var result = std.ArrayList(Machine).init(alloc);
    var blocks = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n\n");
    while (blocks.next()) |block| {
        var numbers = std.mem.tokenizeAny(u8, block, "ButtonABPrizeXY:=+, \n");
        try result.append(.{
            .a = .{ try std.fmt.parseInt(isize, numbers.next().?, 10), try std.fmt.parseInt(isize, numbers.next().?, 10) },
            .b = .{ try std.fmt.parseInt(isize, numbers.next().?, 10), try std.fmt.parseInt(isize, numbers.next().?, 10) },
            .p = .{ try std.fmt.parseInt(isize, numbers.next().?, 10), try std.fmt.parseInt(isize, numbers.next().?, 10) },
        });
    }
    return result;
}

fn compute(a: Vec2, b: Vec2, p: Vec2) !?struct { isize, isize } {
    const u = a[0] * p[1] - a[1] * p[0];
    const v = b[1] * p[0] - b[0] * p[1];
    if (std.math.sign(u) != std.math.sign(v)) return null;
    const gcd_uv = std.math.gcd(@abs(u), @abs(v));
    const u_ = try std.math.divExact(isize, @intCast(@abs(u)), @intCast(gcd_uv));
    const v_ = try std.math.divExact(isize, @intCast(@abs(v)), @intCast(gcd_uv));
    const w = std.math.divExact(isize, p[0], u_ * b[0] + v_ * a[0]) catch return null;
    return .{ v_ * w, u_ * w };
}

fn part1(input: []const u8) !isize {
    const machines = try parse(input);
    defer machines.deinit();

    var cost: isize = 0;
    for (machines.items) |m| {
        if (try compute(m.a, m.b, m.p)) |result|
            cost += 3 * result[0] + result[1];
    }
    return cost;
}

fn part2(input: []const u8) !isize {
    const machines = try parse(input);
    defer machines.deinit();

    var cost: isize = 0;
    for (machines.items) |m| {
        if (try compute(m.a, m.b, m.p + @as(Vec2, @splat(10000000000000)))) |result|
            cost += 3 * result[0] + result[1];
    }
    return cost;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input13.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\Button A: X+94, Y+34
        \\Button B: X+22, Y+67
        \\Prize: X=8400, Y=5400
        \\
        \\Button A: X+26, Y+66
        \\Button B: X+67, Y+21
        \\Prize: X=12748, Y=12176
        \\
        \\Button A: X+17, Y+86
        \\Button B: X+84, Y+37
        \\Prize: X=7870, Y=6450
        \\
        \\Button A: X+69, Y+23
        \\Button B: X+27, Y+71
        \\Prize: X=18641, Y=10279
        \\
    ;
    try std.testing.expectEqual(480, part1(input));
}
