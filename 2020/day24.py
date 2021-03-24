#!/usr/bin/env python
import re

problem = """\
sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
"""

problem = open("input24.txt").read()

DIRS = {
    "e": (2, 0),
    "se": (1, 1),
    "sw": (-1, 1),
    "w": (-2, 0),
    "nw": (-1, -1),
    "ne": (1, -1),
}


# part 1

tiles = {}
for line in problem.splitlines():
    x, y = 0, 0
    for dir in re.finditer("[ns]?[ew]", line):
        Δx, Δy = DIRS[dir.group(0)]
        x, y = x + Δx, y + Δy
    tiles[x, y] = not tiles.get((x, y), False)

print(sum(tiles.values()))


# part 2

for i in range(100):
    x_min = min(x for (x, y), black in tiles.items() if black)
    x_max = max(x for (x, y), black in tiles.items() if black)
    y_min = min(y for (x, y), black in tiles.items() if black)
    y_max = max(y for (x, y), black in tiles.items() if black)
    tiles = {
        (x, y): 0 < adjacent <= 2 if black else adjacent == 2
        for x, y, black, adjacent in (
            (
                x,
                y,
                tiles.get((x, y), False),
                sum(tiles.get((x + Δx, y + Δy), False) for Δx, Δy in DIRS.values()),
            )
            for y in range(y_min - 1, y_max + 2)
            for x in range(x_min - 2 - (y + x_min) % 2, x_max + 3, 2)
        )
    }

print(sum(tiles.values()))
