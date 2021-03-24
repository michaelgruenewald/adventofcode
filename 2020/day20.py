#!/usr/bin/env python
from collections import defaultdict, Counter, namedtuple
from math import prod, isqrt
import re

camera_data = """\
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
"""

camera_data = open("input20.txt").read()


# part 1

EDGE_LEN = 10
SHORT_EDGE_LEN = EDGE_LEN - 2


def edge_id(it):
    return sum(1 << i for i, edge_overlap in enumerate(it) if edge_overlap == "#")


def flip_edge(edge):
    return sum((edge & (1 << i)) << (EDGE_LEN - 1 - i) >> i for i in range(EDGE_LEN))


cameras = {}
camera_bits = {}
for camera_blob in camera_data.split("\n\n"):
    (camera_id,) = re.findall(r"\d+", camera_blob)
    camera_id = int(camera_id)
    tile = camera_blob.splitlines()[1:]
    # add that camera
    cameras[camera_id] = [
        edge_id(edge)
        for edge in (
            tile[0],
            (l[-1] for l in tile),
            tile[-1][::-1],
            (l[0] for l in tile[::-1]),
        )
    ]
    camera_bits[camera_id] = {
        (row, col): c
        for row, l in enumerate(tile[1:-1])
        for col, c in enumerate(l[1:-1])
    }
    # and the flipped version
    cameras[-camera_id] = [
        flip_edge(cameras[camera_id][0]),
        flip_edge(cameras[camera_id][3]),
        flip_edge(cameras[camera_id][2]),
        flip_edge(cameras[camera_id][1]),
    ]
    camera_bits[-camera_id] = {
        (row, SHORT_EDGE_LEN - 1 - col): c
        for (row, col), c in camera_bits[camera_id].items()
    }

edge_to_camera = defaultdict(set)
for camera_id, edges in cameras.items():
    for e in edges:
        edge_to_camera[e].add(camera_id)

ctr = Counter(next(iter(v)) for k, v in edge_to_camera.items() if len(v) == 1)
corners = {abs(c) for c, v in ctr.items() if v >= 2}
assert len(corners) == 4

print(prod(corners), corners)


# part 2

# find the topleft camera, and the top and left edges
topleft = next(iter(corners))
((left, top),) = (
    (x, y)
    for x, y in zip(cameras[topleft], cameras[topleft][1:] + cameras[topleft][:1])
    if len(edge_to_camera[x]) == 1 and len(edge_to_camera[y]) == 1
)

CAMS_PER_SIDE = isqrt(len(cameras) // 2)

Tile = namedtuple("Tile", ("camera", "n", "e", "s", "w", "rotate"))

tiles = {}

for row in range(CAMS_PER_SIDE):
    for col in range(CAMS_PER_SIDE):
        if row == 0 and col == 0:
            n, w, cam = top, left, topleft
        elif row == 0:
            western = tiles[row, col - 1]
            n, w = None, flip_edge(western.e)
            (cam,) = edge_to_camera[w] - {western.camera, -western.camera}
        elif col == 0:
            northern = tiles[row - 1, col]
            n, w = flip_edge(northern.s), None
            (cam,) = edge_to_camera[n] - {northern.camera, -northern.camera}
        else:
            northern = tiles[row - 1, col]
            western = tiles[row, col - 1]
            n, w = flip_edge(northern.s), flip_edge(western.e)
            (cam,) = (edge_to_camera[w] - {western.camera, -western.camera}) & (
                edge_to_camera[n] - {northern.camera, -northern.camera}
            )
        camera = cameras[cam]
        if w:
            e = camera[(camera.index(w) + 2) % 4]
        if not n:
            (n,) = (
                edge
                for edge, cameras in edge_to_camera.items()
                if cameras == {cam} and edge != e
            )
        s = camera[(camera.index(n) + 2) % 4]
        if not w:
            (w,) = (
                edge
                for edge, cameras in edge_to_camera.items()
                if cameras == {cam} and edge != s
            )
            e = camera[(camera.index(w) + 2) % 4]

        rotate = camera.index(n)
        tile = Tile(cam, n, e, s, w, rotate)
        assert tile not in tiles.values()
        tiles[row, col] = tile

pixels = set()
for y in range(CAMS_PER_SIDE * SHORT_EDGE_LEN):
    for x in range(CAMS_PER_SIDE * SHORT_EDGE_LEN):
        row, rowy = divmod(y, EDGE_LEN - 2)
        col, colx = divmod(x, EDGE_LEN - 2)
        tile = tiles[row, col]

        for _ in range(tile.rotate):
            colx, rowy = SHORT_EDGE_LEN - 1 - rowy, colx

        if camera_bits[tile.camera][rowy, colx] == "#":
            pixels.add((y, x))

# for y in range(CAMS_PER_SIDE * SHORT_EDGE_LEN):
#     for x in range(CAMS_PER_SIDE * SHORT_EDGE_LEN):
#         print("#" if (y, x) in pixels else ".", end="")
#     print()

MONSTER = """\
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
"""

monster = {
    (row, col)
    for row, line in enumerate(MONSTER.splitlines())
    for col, c in enumerate(line)
    if c == "#"
}

ROTATE = [
    lambda y, x: (y, x),
    lambda y, x: (x, CAMS_PER_SIDE * SHORT_EDGE_LEN - 1 - y),
    lambda y, x: (
        CAMS_PER_SIDE * SHORT_EDGE_LEN - 1 - y,
        CAMS_PER_SIDE * SHORT_EDGE_LEN - 1 - x,
    ),
    lambda y, x: (CAMS_PER_SIDE * SHORT_EDGE_LEN - 1 - x, y),
]
FLIP = [
    lambda y, x: (y, x),
    lambda y, x: (CAMS_PER_SIDE * SHORT_EDGE_LEN - 1 - y, x),
]

monsters = sum(
    all(rotator(*flipper(y + col, x + row)) in pixels for row, col in monster)
    for rotator in ROTATE
    for flipper in FLIP
    for y in range(CAMS_PER_SIDE * SHORT_EDGE_LEN)
    for x in range(CAMS_PER_SIDE * SHORT_EDGE_LEN)
)

print(len(pixels) - monsters * len(monster), monsters)
