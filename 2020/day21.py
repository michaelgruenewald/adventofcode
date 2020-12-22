#!/usr/bin/env python
from functools import reduce
from collections import defaultdict, Counter
from pprint import pprint
import re

problem = """\
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
"""

problem = open("input21.txt").read()

all_ingreds = defaultdict(int)
groups = defaultdict(set)

for line in problem.splitlines():
    ingred_blob, allergen_blob = re.fullmatch(r"(.*) \(contains (.*)\)", line).groups()
    ingreds = frozenset(ingred_blob.split())
    allergens = frozenset(allergen_blob.split(", "))
    for allergen in allergens:
        groups[allergen].add(ingreds)
    for ingred in ingreds:
        all_ingreds[ingred] += 1

reduced = {allergen: reduce(lambda a, b: a & b, g) for allergen, g in groups.items()}
chosen = {}

while reduced:
    allergen = next(a for a, g in reduced.items() if len(g - set(chosen.values())) == 1)
    (chosen[allergen],) = reduced[allergen] - set(chosen.values())
    del reduced[allergen]

# part 1

safe = set(all_ingreds.keys()) - set(chosen.values())
safe_appears = sum(all_ingreds[ingred] for ingred in safe)
print(safe_appears, len(safe), len(all_ingreds))

# part 2

print(",".join(i for a, i in sorted(chosen.items())))
