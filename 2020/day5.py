#!/usr/bin/env python

lines = """\
FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL
""".splitlines()

lines = open("input5.txt").read().splitlines()


def seatno(line):
    return sum((c in ("B", "R")) * 2 ** i for i, c in enumerate(line[::-1]))


# part 1
print(max(seatno(line) for line in lines))

# part 2
seatnos = {seatno(line) for line in lines}
print(({s + 1 for s in seatnos} & {s - 1 for s in seatnos}) - seatnos)
