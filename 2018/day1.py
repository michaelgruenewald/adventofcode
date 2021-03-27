#!/usr/bin/env python
from itertools import *

lines = list(map(int, open("input1.txt").readlines()))
print(sum(lines))

last = 0
seen = set([last])
for f in cycle(lines):
    last = last + f
    if last in seen:
        break
    seen.add(last)

print(last)
