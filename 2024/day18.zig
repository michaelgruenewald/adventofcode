const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = if (@import("builtin").is_test) std.testing.allocator else gpa.allocator();

const Vec2 = @Vector(2, i16);

fn parse(input: []const u8) !std.AutoArrayHashMap(Vec2, usize) {
    var output = std.AutoArrayHashMap(Vec2, usize).init(alloc);
    var lines = std.mem.splitScalar(u8, std.mem.trim(u8, input, "\n"), '\n');
    var i: usize = 0;
    while (lines.next()) |line| : (i += 1) {
        var it = std.mem.splitScalar(u8, line, ',');
        const x = try std.fmt.parseInt(i16, it.next().?, 10);
        const y = try std.fmt.parseInt(i16, it.next().?, 10);
        try output.put(.{ x, y }, i);
    }
    return output;
}

fn run(map: *std.AutoArrayHashMap(Vec2, usize), end: Vec2, cutoff: usize) !?usize {
    var work = std.AutoArrayHashMap(Vec2, usize).init(alloc);
    defer work.deinit();
    var finalized = std.AutoArrayHashMap(Vec2, void).init(alloc);
    defer finalized.deinit();

    try work.put(.{ 0, 0 }, 0);

    return while (work.count() > 0) {
        const cheapest = std.sort.argMin(usize, work.values(), {}, std.sort.asc(usize)).?;
        const cost = work.values()[cheapest];
        const pos = work.keys()[cheapest];

        if (std.meta.eql(pos, end))
            break cost;

        _ = work.swapRemove(pos);
        try finalized.put(pos, {});

        for ([_]Vec2{ .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } }) |d| {
            if (@reduce(.Or, pos + d < Vec2{ 0, 0 }) or
                @reduce(.Or, pos + d > end) or
                finalized.contains(pos + d))
                continue;

            if (map.get(pos + d)) |l|
                if (l < cutoff)
                    continue;

            const e = try work.getOrPut(pos + d);
            if (e.found_existing or e.value_ptr.* > cost + 1)
                e.value_ptr.* = cost + 1;
        }
    } else null;
}

fn part1(input: []const u8, end: Vec2, cutoff: usize) !usize {
    var map = try parse(input);
    defer map.deinit();

    return (try run(&map, end, cutoff)).?;
}

fn part2(input: []const u8, end: Vec2) !Vec2 {
    var map = try parse(input);
    defer map.deinit();

    for (0..map.count()) |cutoff| {
        if (try run(&map, end, cutoff) == null)
            return map.keys()[cutoff - 1];
    } else unreachable;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(alloc, "input18.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input, .{ 70, 70 }, 1024)});
    std.debug.print("{d}\n", .{try part2(input, .{ 70, 70 })});
}

test "examples" {
    const input =
        \\5,4
        \\4,2
        \\4,5
        \\3,0
        \\2,1
        \\6,3
        \\2,4
        \\1,5
        \\0,6
        \\3,3
        \\2,6
        \\5,1
        \\1,2
        \\5,5
        \\2,5
        \\6,5
        \\1,4
        \\0,4
        \\6,4
        \\1,1
        \\6,1
        \\1,0
        \\0,5
        \\1,6
        \\2,0
        \\
    ;
    try std.testing.expectEqual(22, part1(input, .{ 6, 6 }, 12));
    try std.testing.expectEqual(Vec2{ 6, 1 }, part2(input, .{ 6, 6 }));
}
