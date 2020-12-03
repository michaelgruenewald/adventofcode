#!/usr/bin/env python

data = [int(x) for x in open("input1.txt").read().splitlines()]

print(next(x*y for x in data for y in data if x+y==2020))
print(next(x*y*z for x in data for y in data for z in data if x+y+z==2020))
