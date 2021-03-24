#!/usr/bin/env python
import re

lines = """\
2 * 3 + (4 * 5)
5 + (8 * 3 + 9 + 3 * 4 * 3)
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
""".splitlines()

lines = open("input18.txt").read().splitlines()


# part 1


class Evaluator1:
    def __init__(self, line):
        self.tokens = re.findall(r"[()*+]|\d+", line)

    def shift(self, *, when=None):
        if not when or self.tokens and when(self.tokens[0]):
            return self.tokens.pop(0)

    def nextV(self):
        token = self.shift()
        if token == "(":
            v = self.nextOp()
            br = self.shift()
            assert br == ")"
            return v
        else:
            return int(token)

    def nextOp(self):
        v = self.nextV()
        while op := self.shift(when=lambda e: e in ("+", "*")):
            if op == "+":
                v += self.nextV()
            elif op == "*":
                v *= self.nextV()
        return v

    def evaluate(self):
        v = self.nextOp()
        assert not self.tokens
        return v


results = [Evaluator1(line).evaluate() for line in lines]
print(sum(results))


# part 2


class Evaluator2:
    def __init__(self, line):
        self.tokens = re.findall(r"[()*+]|\d+", line)

    def shift(self, when=None):
        if not when or self.tokens and when(self.tokens[0]):
            return self.tokens.pop(0)

    def nextV(self):
        token = self.shift()
        if token == "(":
            v = self.nextMul()
            br = self.shift()
            assert br == ")"
            return v
        else:
            return int(token)

    def nextAdd(self):
        v = self.nextV()
        while self.shift(when=lambda e: e == "+"):
            v += self.nextV()
        return v

    def nextMul(self):
        v = self.nextAdd()
        while self.shift(when=lambda e: e == "*"):
            v *= self.nextAdd()
        return v

    def evaluate(self):
        v = self.nextMul()
        assert not self.tokens
        return v


results = [Evaluator2(line).evaluate() for line in lines]
print(sum(results))
