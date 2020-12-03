#!/usr/bin/env python
import re

lines = open("input2.txt").read().splitlines()

valid, invalid = 0, 0
for line in lines:
    lower, upper, letter, password = re.match(r'^(\d+)-(\d+) ([a-z]): ([a-z]+)$', line).groups()

    if int(lower) <= password.count(letter) <= int(upper):
        valid += 1
    else:
        invalid += 1

print(valid, invalid)


valid, invalid = 0, 0
for line in lines:
    first, second, letter, password = re.match(r'^(\d+)-(\d+) ([a-z]): ([a-z]+)$', line).groups()

    if (password[int(first)-1] == letter) != (password[int(second)-1] == letter):
        valid += 1
    else:
        invalid += 1

print(valid, invalid)
