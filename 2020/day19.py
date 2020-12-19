#!/usr/bin/env python
from functools import lru_cache
import re

problem = """\
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
"""

problem = """\
42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
"""

problem = open("input19.txt").read()


# part 1

rule_blob, message_blob = problem.split("\n\n")

rules = {}
for line in rule_blob.splitlines():
    n, c, r = re.fullmatch(r'(\d+): (?:"(.)"|(.*))', line).groups()
    if c:
        rules[n] = c
    else:
        rules[n] = [rr.split() for rr in r.split(" | ")]


@lru_cache
def rule2re(n):
    if isinstance(rules[n], str):
        return rules[n]
    return "(" + "|".join("".join(rule2re(x) for x in p) for p in rules[n]) + ")"


matcher = re.compile(rule2re("0"))
print(sum(1 for line in message_blob.splitlines() if matcher.fullmatch(line)))


# part 2

max_len = max(len(line) for line in message_blob.splitlines())


@lru_cache
def rule2re2(n):
    if isinstance(rules[n], str):
        return rules[n]
    elif n == "8":
        return rule2re2("42") + "+"
    elif n == "11":
        # REs don't like a^n+b^n expressions, let's cheat
        r42 = rule2re2("42")
        r31 = rule2re2("31")
        return r42 + f"({r42}" * max_len + f"{r31})?" * max_len + r31
    return "(" + "|".join("".join(rule2re2(x) for x in p) for p in rules[n]) + ")"


matcher = re.compile(rule2re2("0"))
print(sum(1 for line in message_blob.splitlines() if matcher.fullmatch(line)))
