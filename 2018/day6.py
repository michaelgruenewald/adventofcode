#!/usr/bin/env python
import csv
from collections import namedtuple, defaultdict

Point = namedtuple("Point", ["x", "y"])

# i = """1, 1
# 1, 6
# 8, 3
# 3, 4
# 5, 5
# 8, 9""".splitlines()
i = file("input6.txt")

points = [Point(*map(int, d)) for d in csv.reader(i)]

xmin = min(p.x for p in points)
xmax = max(p.x for p in points)
ymin = min(p.y for p in points)
ymax = max(p.x for p in points)

ownedby = defaultdict(list)
area10000 = set()

for x in range(xmin, xmax + 1):
    for y in range(ymin, ymax + 1):
        deltas = sorted((abs(p.x - x) + abs(p.y - y), i) for i, p in enumerate(points))
        if deltas[0][1] != deltas[1][1]:
            ownedby[deltas[0][1]].append(Point(x, y))
        if sum(d[0] for d in deltas) < 10000:
            area10000.add(Point(x, y))

print sorted(
    (len(v), k)
    for k, v in ownedby.items()
    if points[k].x not in [xmin, xmax] and points[k].y not in [ymin, ymax]
)[-1]
print len(area10000)
