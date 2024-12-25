const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

fn part1(input: []const u8) !usize {
    var result: usize = 0;
    var lines = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        var secret = try std.fmt.parseInt(u24, line, 10);
        for (0..2000) |_| {
            secret = secret ^ (secret *% 64);
            secret = secret ^ (secret / 32);
            secret = secret ^ (secret *% 2048);
        }
        result += secret;
    }
    return result;
}

fn part2(input: []const u8) !usize {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    var changes = std.AutoHashMap([4]i16, std.AutoHashMap(usize, i16)).init(arena.allocator());
    defer changes.deinit();

    var lines = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n");
    var buyer: usize = 0;
    while (lines.next()) |line| {
        var secret = try std.fmt.parseInt(u24, line, 10);
        var last_changes = [4]i16{ 0, 0, 0, 0 };
        var price: i16 = 0;

        for (0..2000) |i| {
            const new_price: i16 = @intCast(secret % 10);
            const new_change = new_price - price;
            last_changes = .{ last_changes[1], last_changes[2], last_changes[3], new_change };

            if (i > 3) {
                const e = try changes.getOrPut(last_changes);
                if (!e.found_existing)
                    e.value_ptr.* = std.AutoHashMap(usize, i16).init(arena.allocator());
                _ = try e.value_ptr.*.getOrPutValue(buyer, new_price);
            }

            price = new_price;
            secret = secret ^ (secret *% 64);
            secret = secret ^ (secret / 32);
            secret = secret ^ (secret *% 2048);
        }
        buyer += 1;
    }

    var result: usize = 0;
    var changes_it = changes.iterator();
    while (changes_it.next()) |c| {
        var sum: usize = 0;
        var buyers_it = c.value_ptr.*.iterator();
        while (buyers_it.next()) |b| {
            sum += @intCast(b.value_ptr.*);
        }
        if (sum > result)
            result = sum;
    }
    return result;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input22.txt", ~@as(usize, 0));

    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "examples" {
    try std.testing.expectEqual(37327623, try part1(
        \\1
        \\10
        \\100
        \\2024
        \\
    ));
    try std.testing.expectEqual(23, try part2(
        \\1
        \\2
        \\3
        \\2024
        \\
    ));
}
