#!/usr/bin/env python
import re

lines = open("input2.txt").read().splitlines()

INPUT_PATTERN = re.compile(r"^(\d+)-(\d+) ([a-z]): ([a-z]+)$")

valid, invalid = 0, 0
for line in lines:
    lower, upper, letter, password = INPUT_PATTERN.match(line).groups()

    if int(lower) <= password.count(letter) <= int(upper):
        valid += 1
    else:
        invalid += 1

print(valid, invalid)


valid, invalid = 0, 0
for line in lines:
    first, second, letter, password = INPUT_PATTERN.match(line).groups()

    if (password[int(first) - 1] == letter) != (password[int(second) - 1] == letter):
        valid += 1
    else:
        invalid += 1

print(valid, invalid)
