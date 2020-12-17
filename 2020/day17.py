#!/usr/bin/env python
from collections import defaultdict, namedtuple

initial = """\
.#.
..#
###
"""

initial = open("input17.txt").read()


# part 1

P3D = namedtuple("P3D", ("x", "y", "z"))

state = set(
    P3D(x, y, 0)
    for y, line in enumerate(initial.splitlines())
    for x, char in enumerate(line)
    if char == "#"
)

xmin, xmax = min(p.x for p in state), max(p.x for p in state)
ymin, ymax = min(p.y for p in state), max(p.y for p in state)
zmin, zmax = 0, 0

for iteration in range(6):
    xmin -= 1
    xmax += 1
    ymin -= 1
    ymax += 1
    zmin -= 1
    zmax += 1
    newstate = set()
    for x in range(xmin, xmax + 1):
        for y in range(ymin, ymax + 1):
            for z in range(zmin, zmax + 1):
                active = (x, y, z) in state
                neighbors = (
                    sum(
                        (xx, yy, zz) in state
                        for xx in range(x - 1, x + 2)
                        for yy in range(y - 1, y + 2)
                        for zz in range(z - 1, z + 2)
                    )
                    - active
                )
                if (active and 2 <= neighbors <= 3) or (not active and neighbors == 3):
                    newstate.add(P3D(x, y, z))
    state = newstate

print(len(state))


# part 2

P4D = namedtuple("P4D", ("x", "y", "z", "w"))

state = set(
    P4D(x, y, 0, 0)
    for y, line in enumerate(initial.splitlines())
    for x, char in enumerate(line)
    if char == "#"
)

xmin, xmax = min(p.x for p in state), max(p.x for p in state)
ymin, ymax = min(p.y for p in state), max(p.y for p in state)
zmin, zmax = 0, 0
wmin, wmax = 0, 0

for iteration in range(6):
    xmin -= 1
    xmax += 1
    ymin -= 1
    ymax += 1
    zmin -= 1
    zmax += 1
    wmin -= 1
    wmax += 1
    newstate = set()
    for x in range(xmin, xmax + 1):
        for y in range(ymin, ymax + 1):
            for z in range(zmin, zmax + 1):
                for w in range(wmin, wmax + 1):
                    active = (x, y, z, w) in state
                    neighbors = (
                        sum(
                            (xx, yy, zz, ww) in state
                            for xx in range(x - 1, x + 2)
                            for yy in range(y - 1, y + 2)
                            for zz in range(z - 1, z + 2)
                            for ww in range(w - 1, w + 2)
                        )
                        - active
                    )
                    if (active and 2 <= neighbors <= 3) or (
                        not active and neighbors == 3
                    ):
                        newstate.add(P4D(x, y, z, w))
    state = newstate

print(len(state))
