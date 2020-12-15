#!/usr/bin/env python
from collections import defaultdict

problem = "0,3,6"
problem = "0,20,7,16,1,18,15"


# part 1

stack = [int(x) for x in problem.split(",")]

for i in range(2020 - len(stack)):
    if stack[-1] not in stack[:-1]:
        stack.append(0)
    else:
        stack.append(stack[::-1][1:].index(stack[-1]) + 1)

print(stack[-1])


# part 2

seen = {}


def have_seen(i, n):
    _, x = seen.get(n, (None, None))
    seen[n] = x, i


for i, last in enumerate(int(x) for x in problem.split(",")):
    have_seen(i, last)

for i in range(i + 1, 30000000):
    previous, _ = seen.get(last, (None, None))
    last = i - previous - 1 if previous is not None else 0
    have_seen(i, last)

print(last)
