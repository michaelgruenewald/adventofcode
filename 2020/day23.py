#!/usr/bin/env python
from functools import reduce
from itertools import chain

start = list(map(int, "389125467"))
start = list(map(int, "158937462"))


class Cup(object):
    def __init__(self, label, after):
        self.label = label
        if after:
            self.prev = after
            self.next = after.next
            after.next.prev = self
            after.next = self
        else:
            self.prev = self.next = self

    def drop(self):
        self.prev.next, self.next.prev = self.next, self.prev
        return self.label

    def __repr__(self):
        return f"<Cup {self.label} [{self.prev.label}/{self.next.label}]>"

    def __iter__(self):
        current = self
        while True:
            yield current
            current = current.next
            if current == self:
                break


def run(labels, iterations=100):
    current = reduce(lambda prev, label: Cup(label, prev), labels, None).next
    by_label = {c.label: c for c in current}

    for _ in range(iterations):
        dropped_labels = [current.next.drop() for _ in range(3)]
        dest_label = current.label - 1
        while dest_label in dropped_labels or dest_label < 1:
            dest_label -= 1
            if dest_label < 1:
                dest_label = len(by_label)
        dest = by_label[dest_label]
        for label in dropped_labels:
            by_label[label] = dest = Cup(label, dest)
        current = current.next

    return by_label[1]


# part 1

cup1 = run(start, 100)
print("".join(str(x.label) for x in cup1 if x.label != 1))


# part 2

cup1 = run(chain(start, range(max(start) + 1, 1_000_001)), 10_000_000)
print(cup1.next.label * cup1.next.next.label)
