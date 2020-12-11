#!/usr/bin/env python
from collections import defaultdict
from itertools import count, dropwhile

lines = """\
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
""".splitlines()

lines = open("input11.txt").read().splitlines()

SURROUNDING = ((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))


# part 1

data = {
    (row, col): char
    for row, chars in enumerate(lines)
    for col, char in enumerate(chars)
}

while True:
    newdata = {}
    for (row, col), seat in data.items():
        surrounding = [data.get((row + rd, col + cd)) for rd, cd in SURROUNDING]
        if seat == "L" and "#" not in surrounding:
            newdata[row, col] = "#"
        elif seat == "#" and surrounding.count("#") >= 4:
            newdata[row, col] = "L"
        else:
            newdata[row, col] = seat
    if data == newdata:
        break
    data = newdata

print(list(data.values()).count("#"))


# part 2

data = {
    (row, col): char
    for row, chars in enumerate(lines)
    for col, char in enumerate(chars)
}

while True:
    newdata = {}
    for (row, col), seat in data.items():
        surrounding = [
            next(
                dropwhile(
                    lambda x: x == ".",
                    (data.get((row + rd * i, col + cd * i)) for i in count(1)),
                )
            )
            for rd, cd in SURROUNDING
        ]
        if seat == "L" and "#" not in surrounding:
            newdata[row, col] = "#"
        elif seat == "#" and surrounding.count("#") >= 5:
            newdata[row, col] = "L"
        else:
            newdata[row, col] = seat
    if data == newdata:
        break
    data = newdata

print(list(data.values()).count("#"))
