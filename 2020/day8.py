#!/usr/bin/env python
from functools import reduce

lines = """\
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
""".splitlines()

lines = open("input8.txt").read().splitlines()


# part 1
seen = set()
acc = 0
ip = 0
while ip not in seen:
    seen.add(ip)
    op, num = lines[ip].split()
    if op == "acc":
        acc += int(num)
        ip += 1
    elif op == "jmp":
        ip += int(num)
    else:
        assert op == "nop"
        ip += 1
print(acc)


# part 2
for broken in range(len(lines)):
    seen = set()
    acc = 0
    ip = 0
    while ip < len(lines):
        if ip in seen:
            break
        seen.add(ip)
        op, num = lines[ip].split()
        if ip == broken:
            op = {"jmp": "nop", "nop": "jmp"}.get(op, op)
        if op == "acc":
            acc += int(num)
            ip += 1
        elif op == "jmp":
            ip += int(num)
        else:
            assert op == "nop"
            ip += 1
    else:
        print(acc)
