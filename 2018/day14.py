#!/usr/bin/env python
from array import array

recipes = array("b", [3, 7])
elves = [0, 1]
while len(recipes) < 681901 + 10:
    recipes.extend(int(c) for c in str(sum((recipes[e] for e in elves))))
    elves = [(e + recipes[e] + 1) % len(recipes) for e in elves]
print("".join(map(str, recipes[-10:])))

recipes = array("b", [3, 7])
elves = [0, 1]
while "681901" not in "".join(map(str, recipes[-30:])):
    recipes.extend(int(c) for c in str(sum((recipes[e] for e in elves))))
    elves = [(e + recipes[e] + 1) % len(recipes) for e in elves]
print("".join(map(str, recipes[-30:])).rfind("681901") + len(recipes) - 30)
