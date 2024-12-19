const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

fn parse(input: []const u8) !struct { std.ArrayList([]const u8), std.ArrayList([]const u8) } {
    var blocks = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n\n");

    var patterns = std.ArrayList([]const u8).init(alloc);
    var pattern_it = std.mem.splitSequence(u8, blocks.next().?, ", ");
    while (pattern_it.next()) |p| try patterns.append(p);

    var designs = std.ArrayList([]const u8).init(alloc);
    var design_it = std.mem.splitSequence(u8, blocks.next().?, "\n");
    while (design_it.next()) |p| try designs.append(p);

    return .{ patterns, designs };
}

fn search(patterns: [][]const u8, design: []const u8) bool {
    if (design.len == 0)
        return true;

    for (patterns) |p| {
        if (std.mem.startsWith(u8, design, p) and search(patterns, design[p.len..]))
            return true;
    }

    return false;
}

fn part1(input: []const u8) !usize {
    var patterns, var designs = try parse(input);
    defer patterns.deinit();
    defer designs.deinit();

    var sum: usize = 0;
    for (designs.items) |d| {
        if (search(patterns.items, d))
            sum += 1;
    }

    return sum;
}

fn search2(patterns: [][]const u8, design: []const u8, cache: *std.StringHashMap(usize)) !usize {
    if (design.len == 0)
        return 1;

    if (cache.get(design)) |cached|
        return cached;

    var found: usize = 0;
    for (patterns) |p| {
        if (std.mem.startsWith(u8, design, p))
            found += try search2(patterns, design[p.len..], cache);
    }

    try cache.put(design, found);

    return found;
}

fn part2(input: []const u8) !usize {
    var patterns, var designs = try parse(input);
    defer patterns.deinit();
    defer designs.deinit();

    var cache = std.StringHashMap(usize).init(alloc);
    defer cache.deinit();

    var sum: usize = 0;
    for (designs.items) |d| {
        sum += try search2(patterns.items, d, &cache);
    }

    return sum;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input19.txt", ~@as(usize, 0));
    try std.io.getStdOut().writer().print("{d}\n", .{try part1(input)});
    try std.io.getStdOut().writer().print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\r, wr, b, g, bwu, rb, gb, br
        \\
        \\brwrr
        \\bggr
        \\gbbr
        \\rrbgbr
        \\ubwu
        \\bwurrg
        \\brgr
        \\bbrgwb
        \\
    ;
    try std.testing.expectEqual(6, part1(input));
    try std.testing.expectEqual(16, part2(input));
}
