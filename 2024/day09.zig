const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

const Range = struct { id: ?isize = null, size: isize };
const RangeList = std.DoublyLinkedList(Range);

fn part1(input: []const u8) !isize {
    var work = RangeList{};

    for (input, 0..) |size, idx2| {
        const node = try a.create(RangeList.Node);
        node.data = .{
            .id = if (idx2 % 2 == 0) @intCast(idx2 / 2) else null,
            .size = try std.fmt.charToDigit(size, 10),
        };
        work.append(node);
    }

    var final = RangeList{};
    while (work.popFirst()) |item| {
        if (item.data.id) |_| {
            final.append(item);
        } else {
            var last = work.pop().?;
            while (last.data.id == null) last = work.pop().?;

            const extra = last.data.size - item.data.size;
            if (extra == 0) {
                final.append(last);
            } else if (extra > 0) {
                const data_rest = try a.create(RangeList.Node);
                data_rest.data = .{ .id = last.data.id, .size = extra };
                work.append(data_rest);

                last.data.size = item.data.size;
                final.append(last);
            } else {
                const empty_rest = try a.create(RangeList.Node);
                empty_rest.data = .{ .id = null, .size = -extra };
                work.insertBefore(work.first.?, empty_rest);

                final.append(last);
            }
        }
    }

    var sum: isize = 0;
    var idx: isize = 0;
    var look_at = final.first;
    while (look_at) |item| : (look_at = item.next) {
        sum += @divExact((idx + idx + item.data.size - 1) * item.data.size, 2) * (item.data.id orelse 0);
        idx += item.data.size;
    }

    return sum;
}

fn part2(input: []const u8) !isize {
    var blocks = RangeList{};

    for (input, 0..) |size, idx2| {
        const node = try a.create(RangeList.Node);
        node.data = .{
            .id = if (idx2 % 2 == 0) @intCast(idx2 / 2) else null,
            .size = try std.fmt.charToDigit(size, 10),
        };
        blocks.append(node);
    }

    const files = try a.alloc(*RangeList.Node, (blocks.len + 1) / 2);
    {
        var next_file = blocks.first.?;
        for (0..files.len) |i| {
            files[i] = next_file;
            if (next_file.next) |p| {
                next_file = p.next.?;
            }
        }
    }

    for (0..files.len) |i| {
        const f = files[files.len - i - 1];
        var look_at = blocks.first;
        while (look_at) |item| : (look_at = item.next) {
            if (item == f)
                break;
            if (item.data.id != null or item.data.size < f.data.size)
                continue;

            var new_space = try a.create(RangeList.Node);
            new_space.data = .{ .size = f.data.size };
            blocks.insertAfter(f, new_space);
            blocks.remove(f);
            blocks.insertBefore(item, f);
            item.data.size -= f.data.size;
            break;
        }
    }

    var sum: isize = 0;
    var idx: isize = 0;
    var look_at = blocks.first;
    while (look_at) |item| : (look_at = item.next) {
        sum += @divExact((idx + idx + item.data.size - 1) * item.data.size, 2) * (item.data.id orelse 0);
        idx += item.data.size;
    }

    return sum;
}

pub fn main() !void {
    const input = std.mem.trim(u8, try std.fs.cwd().readFileAlloc(a, "input09.txt", ~@as(usize, 0)), "\n");

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input = "2333133121414131402";
    try std.testing.expectEqual(1928, part1(input));
    try std.testing.expectEqual(2858, part2(input));
}
