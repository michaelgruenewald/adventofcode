#!/usr/bin/env python
import re

lines = """\
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
""".splitlines()

lines = """\
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
""".splitlines()

lines = open("input7.txt").read().splitlines()

LINE_PATTERN = re.compile(r"(.*?) bags contain (?:no other bags|(.*))\.")
CONTAIN_PATTERN = re.compile(r"(\d+) (.*) bags?")

d = {}
for line in lines:
    container, containees = LINE_PATTERN.fullmatch(line).groups()
    if containees:
        d[container] = [
            CONTAIN_PATTERN.fullmatch(x).groups() for x in containees.split(", ")
        ]
    else:
        d[container] = []


# part 1
def can_have_shiny_gold(color):
    return color == "shiny gold" or any(
        can_have_shiny_gold(col) for num, col in d[color]
    )


print(sum(can_have_shiny_gold(c) for c in d.keys()) - 1)


# part 2
def sub_bag_count(color):
    return sum(int(num) * (sub_bag_count(col) + 1) for num, col in d[color])


print(sub_bag_count("shiny gold"))
