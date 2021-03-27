#!/usr/bin/env python

# data = map(int, "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split())
data = list(map(int, open("input8.txt").read().split()))


def extract(l):
    nchild, nmeta = l[0:2]
    metadata = []
    ptr = 2
    for i in range(nchild):
        s, m = extract(l[ptr:])
        ptr += s
        metadata += m
    metadata += l[ptr : ptr + nmeta]

    return ptr + nmeta, metadata


L, M = extract(data)
print(len(data), L, sum(M))


def xxx(l):
    nchild, nmeta = l[0:2]
    if nchild == 0:
        return 2 + nmeta, sum(l[2 : nmeta + 2])

    cvs = {}
    ptr = 2
    for i in range(nchild):
        s, m = xxx(l[ptr:])
        ptr += s
        cvs[i + 1] = m

    refs = l[ptr : ptr + nmeta]
    return ptr + nmeta, sum(cvs.get(r, 0) for r in refs)


print(xxx(data))
