#!/usr/bin/env python
import unittest


def part1(data):
    return next(x * y for x in data for y in data if x + y == 2020)


def part2(data):
    return next(
        x * y * z for x in data for y in data for z in data if x + y + z == 2020
    )


class Tests(unittest.TestCase):
    EXAMPLE = [1721, 979, 366, 299, 675, 1456]

    def test_part1(self):
        self.assertEqual(514579, part1(self.EXAMPLE))

    def test_part2(self):
        self.assertEqual(241861950, part2(self.EXAMPLE))


if __name__ == "__main__":
    input1 = [int(x) for x in open("input1.txt").read().splitlines()]
    print(part1(input1))
    print(part2(input1))
