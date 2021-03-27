#!/usr/bin/env python
from collections import *
from itertools import *
import re


class Claim(namedtuple("Claim", ["id", "x", "y", "w", "h"])):
    def positions(self):
        for x in range(self.x, self.x + self.w):
            for y in range(self.y, self.y + self.h):
                yield x, y


lines = open("input3.txt").readlines()
claims = [
    Claim(*list(map(int, re.match(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", l).groups())))
    for l in lines
]

claimed = defaultdict(int)

for claim in claims:
    for position in claim.positions():
        claimed[position] += 1

print(sum(v > 1 for v in list(claimed.values())))

for claim in claims:
    if all(claimed[position] == 1 for position in claim.positions()):
        print(claim)
