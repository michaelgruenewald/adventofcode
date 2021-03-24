#!/usr/bin/env python
from collections import defaultdict

lines = """\
16
10
15
5
1
11
7
19
6
12
4
""".splitlines()

lines = """\
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
""".splitlines()

lines = open("input10.txt").read().splitlines()

adapters = {int(x) for x in lines}

starts = {0} | adapters

target = max(adapters) + 3
ends = adapters | {target}

connections = {
    to: tuple(from_ for from_ in starts if (to - from_) in (1, 2, 3)) for to in ends
}


# part 1
counts = defaultdict(int)
pos = target
while pos > 0:
    previous = max(connections[pos])
    counts[pos - previous] += 1
    pos = previous

print(counts[1] * counts[3], counts)


# part 2
weights = {}
for to in sorted(ends):
    weights[to] = sum(1 if from_ == 0 else weights[from_] for from_ in connections[to])

print(weights[target])
