#!/usr/bin/env python
from functools import reduce
from operator import and_, or_

lines = """\
abc

a
b
c

ab
ac

a
a
a
a

b
"""

lines = open("input6.txt").read()

groups = [[set(x) for x in s.split("\n") if x] for s in lines.split("\n\n")]

# part 1
print(sum(len(reduce(or_, g)) for g in groups))

# part 2
print(sum(len(reduce(and_, g)) for g in groups))
