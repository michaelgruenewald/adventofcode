#!/usr/bin/env python
from itertools import product
import re

lines = """\
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
""".splitlines()

lines = """\
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
""".splitlines()

lines = open("input14.txt").read().splitlines()


# part 1

mem = {}
for line in lines:
    if m := re.fullmatch(r"mask = ([01X]+)", line):
        (mask,) = m.groups()
        ormask = int(mask.replace("X", "0"), 2)
        andmask = int(mask.replace("X", "1"), 2)
    elif m := re.fullmatch(r"mem\[(\d+)] = (\d+)", line):
        loc, val = m.groups()
        mem[int(loc)] = (int(val) & andmask) | ormask

print(sum(mem.values()))


# part 2


def setbit(bit):
    return lambda i: i | bit


def clearbit(bit):
    return lambda i: i & ~bit


mem = {}
for line in lines:
    if m := re.fullmatch(r"mask = ([01X]+)", line):
        (mask,) = m.groups()
    elif m := re.fullmatch(r"mem\[(\d+)] = (\d+)", line):
        loc, val = m.groups()
        addr = int(loc)
        floats = []
        for i, bit in enumerate(mask[::-1]):
            if bit == "1":
                addr |= 1 << i
            elif bit == "X":
                floats.append(1 << i)

        for mutations in product(*([setbit(m), clearbit(m)] for m in floats)):
            for mutation in mutations:
                addr = mutation(addr)
            mem[addr] = int(val)

print(sum(mem.values()))
