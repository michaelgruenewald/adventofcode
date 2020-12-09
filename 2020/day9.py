#!/usr/bin/env python
lines = """\
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
""".splitlines()
preamble = 5

lines = open("input9.txt").read().splitlines()
preamble = 25

nums = [int(x) for x in lines]

# part 1
invalid = next(
    n
    for i, n in enumerate(nums[preamble:])
    if not any(
        x + y == n for x in nums[i : i + preamble] for y in nums[i : i + preamble]
    )
)
print(invalid)


# part 2
remain = iter(nums)
queue = []

while sum(queue) != invalid:
    if sum(queue) < invalid:
        queue.append(next(remain))
    else:
        del queue[0]
print(min(queue) + max(queue), queue)
