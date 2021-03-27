#!/usr/bin/env python

serial = 9445
print max(
        (
            sum((((x + dx + 10) * (y + dy) + serial) * (x + dx + 10) / 100 % 10 - 5) for dx in xrange(s) for dy in xrange(s)),
            x,
            y,
            s
        )
        for x in xrange(1, 299)
        for y in xrange(1, 299)
        for s in xrange(1, 300-max(x, y))
        )
