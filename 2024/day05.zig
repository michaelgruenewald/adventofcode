const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const a = gpa.allocator();

const PageNo = u8;

const Rule = struct { l: PageNo, r: PageNo };
const Input = struct { rules: []Rule, pages: [][]PageNo };

fn parse(input: []const u8) !Input {
    var blocks = std.mem.splitSequence(u8, std.mem.trim(u8, input, "\n"), "\n\n");

    var rule_lines = std.mem.splitScalar(u8, blocks.next().?, '\n');
    var rules = std.ArrayList(Rule).init(a);
    while (rule_lines.next()) |line| {
        var it = std.mem.splitScalar(u8, line, '|');
        const l = try std.fmt.parseInt(PageNo, it.next().?, 10);
        const r = try std.fmt.parseInt(PageNo, it.next().?, 10);
        try rules.append(.{ .l = l, .r = r });
    }

    var page_lines = std.mem.splitScalar(u8, blocks.next().?, '\n');
    var pages = std.ArrayList([]PageNo).init(a);
    while (page_lines.next()) |line| {
        var it = std.mem.splitScalar(u8, line, ',');
        var pgs = std.ArrayList(PageNo).init(a);
        while (it.next()) |pg| {
            try pgs.append(try std.fmt.parseInt(PageNo, pg, 10));
        }
        try pages.append(pgs.items);
    }

    return .{ .rules = rules.items, .pages = pages.items };
}

fn cmp(context: []Rule, lhs: PageNo, rhs: PageNo) bool {
    for (context) |rule| {
        if (lhs == rule.r and rhs == rule.l)
            return false;
    }
    return true;
}

fn part1(input: []const u8) !usize {
    const i = try parse(input);

    var sum: usize = 0;
    for (i.pages) |pgs| {
        const scratch = try a.dupe(PageNo, pgs);
        std.mem.sort(PageNo, scratch, i.rules, cmp);
        const good = std.mem.eql(PageNo, pgs, scratch);
        if (good)
            sum += scratch[scratch.len / 2];
    }

    return sum;
}

fn part2(input: []const u8) !usize {
    const i = try parse(input);

    var sum: usize = 0;
    for (i.pages) |pgs| {
        const scratch = try a.dupe(PageNo, pgs);
        std.mem.sort(PageNo, scratch, i.rules, cmp);
        const good = std.mem.eql(PageNo, pgs, scratch);
        if (!good)
            sum += scratch[scratch.len / 2];
    }

    return sum;
}

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(a, "input05.txt", ~@as(usize, 0));

    std.debug.print("{d}\n", .{try part1(input)});
    std.debug.print("{d}\n", .{try part2(input)});
}

test "examples" {
    const input =
        \\47|53
        \\97|13
        \\97|61
        \\97|47
        \\75|29
        \\61|13
        \\75|53
        \\29|13
        \\97|29
        \\53|29
        \\61|53
        \\97|53
        \\61|29
        \\47|13
        \\75|47
        \\97|75
        \\47|61
        \\75|61
        \\47|29
        \\75|13
        \\53|13
        \\
        \\75,47,61,53,29
        \\97,61,53,29,13
        \\75,29,13
        \\75,97,47,61,53
        \\61,13,29
        \\97,13,75,29,47
        \\
    ;
    try std.testing.expectEqual(143, part1(input));
    try std.testing.expectEqual(123, part2(input));
}
