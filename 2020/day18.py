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


class Evaluator1(object):
    def __init__(self, line):
        self.tokens = re.findall(r"[()*+]|\d+", line)

    def nextV(self):
        token = self.tokens.pop(0)
        if token == "(":
            v = self.nextOp()
            br = self.tokens.pop(0)
            assert br == ")"
            return v
        else:
            return int(token)

    def nextOp(self):
        v = self.nextV()
        while self.tokens and self.tokens[0] in ("+", "*"):
            op = self.tokens.pop(0)
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


class Evaluator2(object):
    def __init__(self, line):
        self.tokens = re.findall(r"[()*+]|\d+", line)

    def nextV(self):
        token = self.tokens.pop(0)
        if token == "(":
            v = self.nextMul()
            br = self.tokens.pop(0)
            assert br == ")"
            return v
        else:
            return int(token)

    def nextAdd(self):
        v = self.nextV()
        while self.tokens and self.tokens[0] == "+":
            self.tokens.pop(0)
            v += self.nextV()
        return v

    def nextMul(self):
        v = self.nextAdd()
        while self.tokens and self.tokens[0] == "*":
            self.tokens.pop(0)
            v *= self.nextAdd()
        return v

    def evaluate(self):
        v = self.nextMul()
        assert not self.tokens
        return v


results = [Evaluator2(line).evaluate() for line in lines]
print(sum(results))
