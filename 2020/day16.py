#!/usr/bin/env python
from collections import defaultdict, namedtuple
from math import prod
import re

notes = """\
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
"""

notes = """\
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
"""


notes = open("input16.txt").read()


rule_blob, my_ticket_blob, nearby_tickets_blob = notes.split("\n\n")

Rule = namedtuple("Rule", ("name", "ranges"))
rules = [
    Rule(name, [range(int(a), int(b) + 1), range(int(c), int(d) + 1)])
    for name, a, b, c, d in (
        re.fullmatch(r"(.*?): (\d+)-(\d+) or (\d+)-(\d+)", rule).groups()
        for rule in rule_blob.splitlines()
    )
]

my_ticket = [int(x) for x in my_ticket_blob.splitlines()[1].split(",")]
nearby_tickets = [
    [int(x) for x in line.split(",")] for line in nearby_tickets_blob.splitlines()[1:]
]


# part 1

invalid = sum(
    field
    for ticket in nearby_tickets
    for field in ticket
    if all(field not in range for rule in rules for range in rule.ranges)
)
print(invalid)


# part 2

valid_tickets = [
    ticket
    for ticket in nearby_tickets
    if all(
        any(field in range for rule in rules for range in rule.ranges)
        for field in ticket
    )
]

possible = defaultdict(list)
for rule in rules:
    for i in range(len(valid_tickets[0])):
        if all(any(ticket[i] in range for range in rule.ranges) for ticket in valid_tickets):
            possible[i].append(rule.name)

found = {}
while any(possible.values()):
    pos, name = next((pos, names[0]) for pos, names in possible.items() if len(names) == 1)
    found[name] = pos
    for v in possible.values():
        if name in v:
            v.remove(name)

print(prod(my_ticket[idx] for name, idx in found.items() if name.startswith("departure")))
