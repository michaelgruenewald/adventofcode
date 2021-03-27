#!/usr/bin/env python
import re
from itertools import groupby

# lines = """Step C must be finished before step A can begin.
# Step C must be finished before step F can begin.
# Step A must be finished before step B can begin.
# Step A must be finished before step D can begin.
# Step B must be finished before step E can begin.
# Step D must be finished before step E can begin.
# Step F must be finished before step E can begin.
# """.splitlines()
lines = open("input7.txt").readlines()

deps = {
    k: set(v[0] for v in vs)
    for k, vs in groupby(
        sorted((re.findall(r"\b[A-Z]\b", line) for line in lines), key=lambda x: x[1]),
        lambda x: x[1],
    )
}

steps = set(deps.keys()) | set(v for vs in list(deps.values()) for v in vs)
done = list()


workers = []
t = 0
while len(done) != len(steps):
    try:
        while len(workers) < 5:
            nextstep = next(
                (
                    step
                    for step in steps
                    if step not in done
                    and step not in (w[1] for w in workers)
                    and all(d in done for d in deps.get(step, set()))
                )
            )
            workers.append((ord(nextstep) - 64 + t + 60, nextstep))
    except StopIteration:
        pass  # Ok
    worker_done = min(workers)
    workers.remove(worker_done)
    t, step = worker_done
    done.append(step)

print("".join(done), t)
