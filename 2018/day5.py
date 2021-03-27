#!/usr/bin/env python

source = list(open("input5.txt").read().strip())

stack = []
for c in source:
    stack.append(c)
    while len(stack) >= 2:
        if stack[-1].lower() == stack[-2].lower() and stack[-1] != stack[-2]:
            stack.pop()
            stack.pop()
        else:
            break

print(len(stack))


types = set(c.lower() for c in source)
results = []
for t in types:
    stack = []
    for c in source:
        if c.lower() != t:
            stack.append(c)
        while len(stack) >= 2:
            if stack[-1].lower() == stack[-2].lower() and stack[-1] != stack[-2]:
                stack.pop()
                stack.pop()
            else:
                break
    results.append((len(stack), t))

print(min(results))
