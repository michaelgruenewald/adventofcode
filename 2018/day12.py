#!/usr/bin/env python
import re

with open("input12.txt") as f:
    state = re.findall("[#.]", f.readline())
    assert f.readline() == "\n"
    patterns = dict(re.match("(.....) => (.)", l).groups() for l in f.readlines())

offset = 0
seen = {}

target = 50000000000

for gen in range(target):
    while state[:4] != [".", ".", ".", "."]:
        state.insert(0, ".")
        offset -= 1
    while state[-4:] != [".", ".", ".", "."]:
        state.append(".")

    new_state = state[:2]
    for i in range(0, len(state) - 1):
        new_state.append(patterns.get("".join(state[i : i + 5]), "."))

    while new_state[:5] == [".", ".", ".", ".", "."]:
        new_state.pop(0)
        offset += 1

    state = new_state

    t = tuple(new_state)
    if t in seen:
        oldgen, oldoffset = seen[t]
        print(
            "Current gen %d offset %d, old gen %d offset %d"
            % (gen, offset, oldgen, oldoffset)
        )

        assert gen - oldgen == 1
        offset += (target - gen - 1) * (offset - oldoffset)
        gen = target - 1
        break
    else:
        seen[t] = (gen, offset)

print(gen, sum(offset + n for n, x in enumerate(state) if x == "#"))
