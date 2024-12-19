const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Opcode = enum(u3) { Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv };
const Instruction = struct { opcode: Opcode, operand: u3 };
const State = struct {
    ip: usize = 0,

    a: usize = 0,
    b: usize = 0,
    c: usize = 0,

    fn combo(self: *@This(), v: u3) usize {
        return switch (v) {
            0...3 => v,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            7 => unreachable,
        };
    }

    fn run(self: *@This(), instructions: []const u3) !std.ArrayList(u3) {
        var output = std.ArrayList(u3).init(alloc);
        while (self.ip < instructions.len) {
            const operand = instructions[self.ip + 1];
            switch (@as(Opcode, @enumFromInt(instructions[self.ip]))) {
                .Adv => self.a = self.a / std.math.pow(usize, 2, self.combo(operand)),
                .Bdv => self.b = self.a / std.math.pow(usize, 2, self.combo(operand)),
                .Cdv => self.c = self.a / std.math.pow(usize, 2, self.combo(operand)),

                .Bst => self.b = self.combo(operand) % 8,

                .Bxl => self.b ^= operand,
                .Bxc => self.b ^= self.c,

                .Jnz => if (self.a != 0) {
                    self.ip = operand;
                    continue;
                },
                .Out => try output.append(@truncate(self.combo(operand))),
            }
            self.ip += 2;
        }
        return output;
    }
};

const NumberIterator = struct {
    slice: []const u8,
    i: usize = 0,

    fn next(self: *@This()) ?usize {
        const start = std.mem.indexOfAnyPos(u8, self.slice, self.i, "0123456789") orelse return null;
        self.i = std.mem.indexOfNonePos(u8, self.slice, start, "0123456789") orelse self.slice.len;
        return std.fmt.parseInt(usize, self.slice[start..self.i], 10) catch unreachable;
    }
};

fn parse(input: []const u8) !struct { State, std.ArrayList(u3) } {
    var it = NumberIterator{ .slice = input };
    const state = State{ .a = it.next().?, .b = it.next().?, .c = it.next().? };
    var instructions = std.ArrayList(u3).init(alloc);
    while (it.next()) |n|
        try instructions.append(@intCast(n));
    return .{ state, instructions };
}

fn part1(input: []const u8) ![]const u8 {
    var state, var instructions = try parse(input);
    defer instructions.deinit();
    var output = try state.run(instructions.items);
    defer output.deinit();

    var result = try alloc.alloc(u8, output.items.len * 2 - 1);
    for (output.items, 0..) |o, i| {
        result[i * 2] = std.fmt.digitToChar(@intCast(o), .lower);
        if (i > 0) result[i * 2 - 1] = ',';
    }

    return result;
}

fn find_digits(state: State, instrs: []const u3, result: usize, digit: usize) !?usize {
    for (0..8) |d| {
        const guess = result + d * std.math.pow(usize, 2, digit * 3);

        var new_state = state;
        new_state.a = guess;
        var output = try new_state.run(instrs);
        defer output.deinit();

        if ((if (output.items.len > digit) output.items[digit] else 0) == instrs[digit]) {
            if (digit == 0)
                return guess
            else if (try find_digits(state, instrs, guess, digit - 1)) |sol|
                return sol;
        }
    }
    return null;
}

fn part2(input: []const u8) !usize {
    const state, var instructions = try parse(input);
    defer instructions.deinit();

    return (try find_digits(state, instructions.items, 0, instructions.items.len - 1)).?;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input17.txt", ~@as(usize, 0));
    try std.io.getStdOut().writer().print("{s}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "machine examples" {
    {
        var s = State{ .c = 9 };
        var o = try s.run(&[_]u3{ 2, 6 });
        defer o.deinit();
        try std.testing.expectEqual(1, s.b);
    }
    {
        var s = State{ .a = 10 };
        var o = try s.run(&[_]u3{ 5, 0, 5, 1, 5, 4 });
        defer o.deinit();
        try std.testing.expectEqualSlices(u3, &[_]u3{ 0, 1, 2 }, o.items);
    }
    {
        var s = State{ .a = 2024 };
        var o = try s.run(&[_]u3{ 0, 1, 5, 4, 3, 0 });
        defer o.deinit();
        try std.testing.expectEqualSlices(u3, &[_]u3{ 4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0 }, o.items);
        try std.testing.expectEqual(0, s.a);
    }
    {
        var s = State{ .b = 29 };
        var o = try s.run(&[_]u3{ 1, 7 });
        defer o.deinit();
        try std.testing.expectEqual(26, s.b);
    }
    {
        var s = State{ .b = 2024, .c = 43690 };
        var o = try s.run(&[_]u3{ 4, 0 });
        defer o.deinit();
        try std.testing.expectEqual(44354, s.b);
    }
}

test "part 1" {
    const input =
        \\Register A: 729
        \\Register B: 0
        \\Register C: 0
        \\
        \\Program: 0,1,5,4,3,0
        \\
    ;
    const result = try part1(input);
    defer alloc.free(result);
    try std.testing.expectEqualStrings("4,6,3,5,6,3,5,2,1,0", result);
}

test "part 2" {
    const input =
        \\Register A: 2024
        \\Register B: 0
        \\Register C: 0
        \\
        \\Program: 0,3,5,4,3,0
    ;
    try std.testing.expectEqual(117440, try part2(input));
}
