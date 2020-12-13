#!/usr/bin/env python
from collections import namedtuple
from itertools import count

lines = """\
939
7,13,x,x,59,x,31,19
""".splitlines()

lines = open("input13.txt").read().splitlines()

time = int(lines[0])
buses = [int(s) for s in lines[1].split(",") if s != "x"]


# part 1

departures = {time + (bus - time % bus): bus for bus in buses}
nextdeparture = min(departures.keys())
nextbus = departures[nextdeparture]

print((nextdeparture - time) * nextbus)


# part 2

Bus = namedtuple("Bus", ("id", "offset"))
buses = [Bus(int(s), offset) for offset, s in enumerate(lines[1].split(",")) if s != "x"]

t, d = 0, 1
for bus in buses:
    # all departures of this bus that fit the pattern with the previous busses
    overlaps = (ts for ts in count(t, d) if (ts + bus.offset) % bus.id == 0)
    # find the first two of them
    t1 = next(overlaps)
    t2 = next(overlaps)
    # use the first one as the new starting point, and the delta for the next search
    t, d = t1, t2 - t1

print(t)
