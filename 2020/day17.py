#!/usr/bin/env python
from collections import defaultdict, namedtuple
from functools import reduce
from itertools import product

initial = """\
.#.
..#
###
"""

initial = open("input17.txt").read()


def compute(state, iterations, rule):
    bounds = reduce(
        lambda a, e: tuple(
            (min(lower, v), max(upper, v)) for (lower, upper), v in zip(a, e)
        ),
        state,
        zip(next(iter(state)), next(iter(state))),
    )

    for iteration in range(iterations):
        bounds = [(min - 1, max + 1) for min, max in bounds]
        state = {
            coord
            for coord in product(*(range(min, max + 1) for min, max in bounds))
            if rule(
                coord in state,
                sum(
                    neighbor_coord in state
                    for neighbor_coord in product(*(range(v - 1, v + 2) for v in coord))
                    if neighbor_coord != coord
                ),
            )
        }

    return len(state)


# part 1

print(
    compute(
        {
            (x, y, 0)
            for y, line in enumerate(initial.splitlines())
            for x, char in enumerate(line)
            if char == "#"
        },
        6,
        lambda active, neighbors: 2 <= neighbors <= 3 if active else neighbors == 3,
    )
)


# part 2

print(
    compute(
        {
            (x, y, 0, 0)
            for y, line in enumerate(initial.splitlines())
            for x, char in enumerate(line)
            if char == "#"
        },
        6,
        lambda active, neighbors: 2 <= neighbors <= 3 if active else neighbors == 3,
    )
)
