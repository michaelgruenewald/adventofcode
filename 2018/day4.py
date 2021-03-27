#!/usr/bin/env python
from collections import *
from itertools import *
import re

lines = sorted(file("input4.txt").readlines())
# lines = """[1518-11-01 00:00] Guard #10 begins shift
# [1518-11-01 00:05] falls asleep
# [1518-11-01 00:25] wakes up
# [1518-11-01 00:30] falls asleep
# [1518-11-01 00:55] wakes up
# [1518-11-01 23:58] Guard #99 begins shift
# [1518-11-02 00:40] falls asleep
# [1518-11-02 00:50] wakes up
# [1518-11-03 00:05] Guard #10 begins shift
# [1518-11-03 00:24] falls asleep
# [1518-11-03 00:29] wakes up
# [1518-11-04 00:02] Guard #99 begins shift
# [1518-11-04 00:36] falls asleep
# [1518-11-04 00:46] wakes up
# [1518-11-05 00:03] Guard #99 begins shift
# [1518-11-05 00:45] falls asleep
# [1518-11-05 00:55] wakes up""".splitlines()

s = None
sleeppatterns = defaultdict(lambda: defaultdict(int))
for l in lines:
    m = re.match(r".*Guard #(\d+)", l)
    if m:
        guardid = m.group(1)
        continue
    m = re.match(r".*:(\d+)] (falls asleep|wakes up)", l)
    if not m:
        1/0
    if m.group(2) == "falls asleep" and s is None:
        s = int(m.group(1))
    elif m.group(2) == "wakes up" and s is not None:
        for i in range(s, int(m.group(1))):
            sleeppatterns[guardid][i] += 1
        s = None
    else:
        2/0

guardstats = [(sum(p.values()), max(p.items(), key=lambda x: x[1])[0], guardid) for guardid, p in sleeppatterns.items()]
print max(guardstats)

guardstats2 = [(max(p.items(), key = lambda x: x[1]), guardid) for guardid, p in sleeppatterns.items()]
print max(guardstats2, key = lambda x: x[0][1])
