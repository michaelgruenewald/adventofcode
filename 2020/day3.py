#!/usr/bin/env python
import math

lines = """\
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#""".splitlines()

lines = open("input3.txt").read().splitlines()

# part 1
print(sum(line[(i * 3) % len(line)] == "#" for i, line in enumerate(lines)))

# part 2
results = []
for right, down in ((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)):
    results.append(
        sum(
            line[(i * right) % len(line)] == "#" for i, line in enumerate(lines[::down])
        )
    )
print(math.prod(results), results)
