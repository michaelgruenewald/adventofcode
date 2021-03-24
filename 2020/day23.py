#!/usr/bin/env python
from functools import reduce
from itertools import chain

start = list(map(int, "389125467"))
start = list(map(int, "158937462"))


class Cup:
    def __init__(self, label, after):
        self.label = label
        if after:
            self.index = after.index
            assert label not in self.index
            self.prev = after
            self.next = after.next
            after.next.prev = self
            after.next = self
        else:
            self.index = {}
            self.prev = self.next = self
        self.index[label] = self

    def drop(self):
        self.prev.next, self.next.prev = self.next, self.prev
        del self.index[self.label]
        return self.label

    def other(self, label):
        return self.index[label]

    def __iter__(self):
        yield self
        current = self.next
        while current is not self:
            yield current
            current = current.next


def run(labels, iterations=100):
    current = reduce(lambda prev, label: Cup(label, prev), labels, None).next
    highest = max(cup.label for cup in current)

    for _ in range(iterations):
        picked = [current.next.drop() for _ in range(3)]
        dest = current.label - 1
        while dest in picked or dest < 1:
            dest = highest if dest == 0 else dest - 1
        reduce(lambda after, label: Cup(label, after), picked, current.other(dest))
        current = current.next

    return current.other(1)


# part 1

cup1 = run(start, 100)
print("".join(str(x.label) for x in cup1 if x.label != 1))


# part 2

cup1 = run(chain(start, range(max(start) + 1, 1_000_001)), 10_000_000)
print(cup1.next.label * cup1.next.next.label)
