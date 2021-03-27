#!/usr/bin/env python
from itertools import count
import re

lines = file("input10.txt").readlines()
lights = [map(int, re.findall("-?\d+", l)) for l in lines]

for t in count():
    grid = set((x + dx * t, y + dy * t) for (x, y, dx, dy) in lights)
    connectedness = sum(((x+1, y) in grid) + ((x-1, y) in grid) + ((x, y+1) in grid) + ((x, y-1) in grid) for (x, y) in grid)

    if connectedness > len(grid):
        print t
        print
        xmin = min(x for (x, y) in grid)
        xmax = max(x for (x, y) in grid)
        ymin = min(y for (x, y) in grid)
        ymax = max(y for (x, y) in grid)

        for y in range(ymin, ymax+1):
            for x in range(xmin, xmax+1):
                if (x, y) in grid:
                    print "x",
                else:
                    print " ",
            print
        print
        break
