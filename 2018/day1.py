#!/usr/bin/env python
from itertools import *

lines = map(int, file("input1.txt").readlines())
print sum(lines)

last = 0
seen = set([last])
for f in cycle(lines):
    last = last + f
    if last in seen:
        break
    seen.add(last)

print last
