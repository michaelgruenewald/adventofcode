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

all_ingredients = defaultdict(int)
allergen_groups = defaultdict(set)

for line in problem.splitlines():
    ingredient_blob, allergen_blob = re.fullmatch(
        r"(.*) \(contains (.*)\)", line
    ).groups()
    ingredients = frozenset(ingredient_blob.split())
    allergens = frozenset(allergen_blob.split(", "))
    for allergen in allergens:
        allergen_groups[allergen].add(ingredients)
    for ingredient in ingredients:
        all_ingredients[ingredient] += 1

allergen_candidates = {
    allergen: reduce(lambda a, b: a & b, groups)
    for allergen, groups in allergen_groups.items()
}
chosen = {}

while allergen_candidates:
    allergen = next(
        allergen
        for allergen, candidates in allergen_candidates.items()
        if len(candidates.difference(chosen.values())) == 1
    )
    (chosen[allergen],) = allergen_candidates[allergen].difference(chosen.values())
    del allergen_candidates[allergen]

# part 1

safe = set(all_ingredients.keys()).difference(chosen.values())
safe_appears = sum(all_ingredients[ingredient] for ingredient in safe)
print(safe_appears, len(safe), len(all_ingredients))

# part 2

print(",".join(i for a, i in sorted(chosen.items())))
