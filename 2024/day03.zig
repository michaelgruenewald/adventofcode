const std = @import("std");
const c = @cImport({
    @cInclude("pcre.h");
});

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

fn PcreExecIterator(GROUPS: comptime_int) type {
    return struct {
        pattern: *const PcrePattern(GROUPS),
        subject: []const u8,
        start: c_int = 0,

        fn next(self: *PcreExecIterator(GROUPS)) ?[GROUPS]?[]const u8 {
            var output: [GROUPS * 3]c_int = undefined;
            const pairs_set = c.pcre_exec(self.pattern.inner, null, self.subject.ptr, @intCast(self.subject.len), self.start, 0, &output, output.len);
            if (pairs_set < 1) return null;
            self.start = output[1];
            var groups: [GROUPS]?[]const u8 = .{null} ** GROUPS;
            for (0..GROUPS) |i| {
                if (i < pairs_set and output[i * 2] != -1)
                    groups[i] = self.subject[@intCast(output[i * 2])..@intCast(output[i * 2 + 1])];
            }
            return groups;
        }
    };
}

fn PcrePattern(GROUPS: comptime_int) type {
    return struct {
        inner: *c.pcre,

        fn init(pattern: [*:0]const u8) !PcrePattern(GROUPS) {
            var errptr: [*:0]allowzero u8 = undefined;
            var erroffset: c_int = undefined;

            if (c.pcre_compile(pattern, 0, &errptr, &erroffset, null)) |compiled| {
                return PcrePattern(GROUPS){ .inner = compiled };
            } else {
                return error.PcreCompileError;
            }
        }

        fn deinit(self: PcrePattern(GROUPS)) void {
            c.pcre_free.?(self.inner);
        }

        fn exec(self: *const PcrePattern(GROUPS), subject: []const u8) PcreExecIterator(GROUPS) {
            return PcreExecIterator(GROUPS){ .pattern = self, .subject = subject };
        }
    };
}

fn part1(input: []const u8) !usize {
    const pattern = try PcrePattern(3).init("mul\\((\\d+),(\\d+)\\)");
    var it = pattern.exec(input);

    var sum: usize = 0;
    while (it.next()) |groups| {
        const x = try std.fmt.parseInt(usize, groups[1].?, 10);
        const y = try std.fmt.parseInt(usize, groups[2].?, 10);
        sum += x * y;
    }
    return sum;
}

fn part2(input: []const u8) !usize {
    const pattern = try PcrePattern(5).init("mul\\((\\d+),(\\d+)\\)|(do\\(\\))|(don't\\(\\))");
    var it = pattern.exec(input);

    var enabled = true;
    var sum: usize = 0;
    while (it.next()) |groups| {
        if (groups[3] != null) {
            enabled = true;
        } else if (groups[4] != null) {
            enabled = false;
        } else if (enabled) {
            const x = try std.fmt.parseInt(usize, groups[1].?, 10);
            const y = try std.fmt.parseInt(usize, groups[2].?, 10);
            sum += x * y;
        }
    }
    return sum;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input03.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    try std.testing.expectEqual(161, part1(input1));
    const input2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
    try std.testing.expectEqual(48, part2(input2));
}
