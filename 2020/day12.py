#!/usr/bin/env python

lines = """\
F10
N3
F7
R90
F11
""".splitlines()

lines = open("input12.txt").read().splitlines()

CARDINALS = {"N": (0, 1), "E": (1, 0), "S": (0, -1), "W": (-1, 0)}
DIRS = list(CARDINALS.keys())


# part 1

shipN, shipE, shipDir = 0, 0, 1

for line in lines:
    if line[0] in CARDINALS.keys():
        shipN += CARDINALS[line[0]][0] * int(line[1:])
        shipE += CARDINALS[line[0]][1] * int(line[1:])
    elif line[0] in ("L", "R"):
        shipDir += int(line[1:]) // 90 * (1 if line[0] == "R" else 3)
        shipDir %= len(DIRS)
    elif line[0] == "F":
        shipN += CARDINALS[DIRS[shipDir]][0] * int(line[1:])
        shipE += CARDINALS[DIRS[shipDir]][1] * int(line[1:])

print(abs(shipN) + abs(shipE), (shipN, shipE))


# part 2

shipN, shipE = 0, 0
wpN, wpE = 10, 1

for line in lines:
    if line[0] in CARDINALS.keys():
        wpN += CARDINALS[line[0]][0] * int(line[1:])
        wpE += CARDINALS[line[0]][1] * int(line[1:])
    elif line[0] == "F":
        shipN += wpN * int(line[1:])
        shipE += wpE * int(line[1:])
    elif line[0] in ("L", "R"):
        for _ in range(int(line[1:]) // 90 * (1 if line[0] == "R" else 3)):
            wpN, wpE = wpE, -wpN

print(abs(shipN) + abs(shipE), (shipN, shipE))
