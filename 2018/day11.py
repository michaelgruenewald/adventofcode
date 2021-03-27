#!/usr/bin/env python

serial = 9445
print(
    max(
        (
            sum(
                (((x + dx + 10) * (y + dy) + serial) * (x + dx + 10) / 100 % 10 - 5)
                for dx in range(s)
                for dy in range(s)
            ),
            x,
            y,
            s,
        )
        for x in range(1, 299)
        for y in range(1, 299)
        for s in range(1, 300 - max(x, y))
    )
)
