#!/usr/bin/env python
from itertools import *

lines = open("input2.txt").readlines()

any2 = sum(any(sum(x == l for x in w) == 2 for l in w) for w in lines)
any3 = sum(any(sum(x == l for x in w) == 3 for l in w) for w in lines)

print(any2, any3, any2 * any3)

for w1 in lines:
    for w2 in lines:
        if sum(c1 != c2 for (c1, c2) in zip(w1, w2)) == 1:
            print(w1, w2)
