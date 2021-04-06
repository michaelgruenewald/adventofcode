#!/usr/bin/env python
from copy import deepcopy
from itertools import count
from unittest import TestCase

EXAMPLE0 = """\
#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######
"""

EXAMPLE1 = """\
#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######
"""

EXAMPLE2 = """\
#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######
"""

EXAMPLE3 = """\
#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######
"""

EXAMPLE4 = """\
#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######
"""

EXAMPLE5 = """\
#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########
"""


class Unit:
    def __init__(self):
        self.hit_points = 200


class Elf(Unit):
    def __str__(self):
        return "E"


class Goblin(Unit):
    def __str__(self):
        return "G"


class Wall:
    def __str__(self):
        return "#"


def adjacent(p):
    return {(p[0] + r, p[1] + c) for r, c in ((0, 1), (0, -1), (1, 0), (-1, 0))}


class Combat:
    def __init__(self, data):
        self.field = {}
        for row, s in enumerate(data.splitlines()):
            for col, c in enumerate(s):
                if c == "E":
                    self.field[row, col] = Elf()
                elif c == "G":
                    self.field[row, col] = Goblin()
                elif c == "#":
                    self.field[row, col] = Wall()
                else:
                    assert c == "."
        self.height = row + 1
        self.width = col + 1

    def print(self):
        for row in range(self.height):
            for col in range(self.width):
                print(self.field.get((row, col), "."), end="")
            print()

    def tick(self, elf_attack_power=3):
        for unit, state in sorted(self._find(Unit).items()):
            if state is not self.field.get(unit):
                # unit died already
                assert state.hit_points <= 0
                continue
            else:
                assert state.hit_points > 0

            if isinstance(self.field[unit], Elf):
                enemies = self._find(Goblin).keys()
                attack_power = elf_attack_power
            else:
                enemies = self._find(Elf).keys()
                attack_power = 3
            assert unit not in enemies

            if not enemies:
                # no enemies left, we won
                return self.field[unit]

            if not enemies & adjacent(unit):
                # no enemies directly adjacent, move towards one

                # empty squares we can move to
                options = self._adjacent_free({unit})
                # in range squares
                in_range = self._adjacent_free(enemies)

                reachable = set()
                for candidate in in_range:
                    targets = {candidate}
                    dist = 0
                    seen = {candidate}
                    while not options & targets:
                        targets = self._adjacent_free(targets) - seen
                        if not targets:
                            break
                        seen |= targets
                        dist += 1
                    else:
                        reachable.add((dist, candidate, min(options & targets)))

                if reachable:
                    _, _, goto = min(reachable)
                    assert goto not in self.field
                    self.field[goto] = self.field[unit]
                    del self.field[unit]
                    unit = goto

            if enemies & adjacent(unit):
                enemy = min(
                    enemies & adjacent(unit),
                    key=lambda p: (self.field[p].hit_points, p),
                )
                self.field[enemy].hit_points -= attack_power
                if self.field[enemy].hit_points <= 0:
                    del self.field[enemy]

    def run(self, *tick_args):
        for i in count():
            won = self.tick(*tick_args)
            if won:
                return won, i, self.hit_points_remaining()

    def count(self, cls):
        return len(self._find(cls))

    def hit_points_remaining(self):
        return sum(u.hit_points for u in self.field.values() if isinstance(u, Unit))

    def _adjacent_free(self, positions):
        return {a for p in positions for a in adjacent(p) if a not in self.field}

    def _find(self, cls):
        return {p: s for p, s in self.field.items() if isinstance(s, cls)}


class Tests(TestCase):
    def test_part1_example0(self):
        winner, rounds, hpr = Combat(EXAMPLE0).run()
        self.assertIsInstance(winner, Goblin)
        self.assertEqual(rounds, 47)
        self.assertEqual(hpr, 590)

    def test_part1_example1(self):
        winner, rounds, hpr = Combat(EXAMPLE1).run()
        self.assertIsInstance(winner, Elf)
        self.assertEqual(rounds, 37)
        self.assertEqual(hpr, 982)

    def test_part1_example2(self):
        winner, rounds, hpr = Combat(EXAMPLE2).run()
        self.assertIsInstance(winner, Elf)
        self.assertEqual(rounds, 46)
        self.assertEqual(hpr, 859)

    def test_part1_example3(self):
        winner, rounds, hpr = Combat(EXAMPLE3).run()
        self.assertIsInstance(winner, Goblin)
        self.assertEqual(rounds, 35)
        self.assertEqual(hpr, 793)

    def test_part1_example4(self):
        winner, rounds, hpr = Combat(EXAMPLE4).run()
        self.assertIsInstance(winner, Goblin)
        self.assertEqual(rounds, 54)
        self.assertEqual(hpr, 536)

    def test_part1_example5(self):
        winner, rounds, hpr = Combat(EXAMPLE5).run()
        self.assertIsInstance(winner, Goblin)
        self.assertEqual(rounds, 20)
        self.assertEqual(hpr, 937)

    def test_part2_example0(self):
        combat = Combat(EXAMPLE0)
        need_elves = combat.count(Elf)
        for eap in count(3):
            c = deepcopy(combat)
            winner, rounds, hpr = c.run(eap)
            if c.count(Elf) == need_elves:
                break
        self.assertIsInstance(winner, Elf)
        self.assertEqual(rounds, 29)
        self.assertEqual(hpr, 172)
        self.assertEqual(eap, 15)

    def test_part2_example2(self):
        combat = Combat(EXAMPLE2)
        need_elves = combat.count(Elf)
        for eap in count(3):
            c = deepcopy(combat)
            winner, rounds, hpr = c.run(eap)
            if c.count(Elf) == need_elves:
                break
        self.assertIsInstance(winner, Elf)
        self.assertEqual(rounds, 33)
        self.assertEqual(hpr, 948)
        self.assertEqual(eap, 4)

    def test_part2_example3(self):
        combat = Combat(EXAMPLE3)
        need_elves = combat.count(Elf)
        for eap in count(3):
            c = deepcopy(combat)
            winner, rounds, hpr = c.run(eap)
            if c.count(Elf) == need_elves:
                break
        self.assertIsInstance(winner, Elf)
        self.assertEqual(rounds, 37)
        self.assertEqual(hpr, 94)
        self.assertEqual(eap, 15)

    def test_part2_example4(self):
        combat = Combat(EXAMPLE4)
        need_elves = combat.count(Elf)
        for eap in count(3):
            c = deepcopy(combat)
            winner, rounds, hpr = c.run(eap)
            if c.count(Elf) == need_elves:
                break
        self.assertIsInstance(winner, Elf)
        self.assertEqual(rounds, 39)
        self.assertEqual(hpr, 166)
        self.assertEqual(eap, 12)

    def test_part2_example5(self):
        combat = Combat(EXAMPLE5)
        need_elves = combat.count(Elf)
        for eap in count(3):
            c = deepcopy(combat)
            winner, rounds, hpr = c.run(eap)
            if c.count(Elf) == need_elves:
                break
        self.assertIsInstance(winner, Elf)
        self.assertEqual(rounds, 30)
        self.assertEqual(hpr, 38)
        self.assertEqual(eap, 34)


if __name__ == "__main__":
    # part 1
    combat = Combat(open("input15.txt").read())
    winner, rounds, hpr = combat.run()
    print(
        f"{winner} won after {rounds} rounds with {hpr} hit points remaining, score {rounds * hpr}"
    )

    # part 2
    combat = Combat(open("input15.txt").read())
    need_elves = combat.count(Elf)
    for eap in count(3):
        c = deepcopy(combat)
        winner, rounds, hpr = c.run(eap)
        if c.count(Elf) == need_elves:
            break
    print(
        f"{winner} won after {rounds} rounds with {hpr} hit points remaining, score {rounds * hpr}; eap {eap}"
    )
