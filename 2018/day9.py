#!/usr/bin/env python

LEFT = 1
RIGHT = 2


def simulate(players, last):
    """
    >>> max(simulate(9, 25))
    32
    >>> simulate(9, 25)
    [0, 0, 0, 0, 32, 0, 0, 0, 0]

    >>> max(simulate(10, 1618))
    8317
    >>> simulate(10, 1618)
    [7738, 6857, 6395, 7896, 7173, 6996, 7932, 7552, 7503, 8317]

    >>> max(simulate(13, 7999))
    146373
    >>> max(simulate(17, 1104))
    2764
    >>> max(simulate(21, 6111))
    54718
    >>> max(simulate(30, 5807))
    37305

    >>> max(simulate(411, 72059))
    429943
    >>> max(simulate(411, 7205900))
    3615691746
    """

    current = [0]
    current.extend((current, current))

    scores = [0] * players

    for n in range(1, last + 1):
        if n % 23 == 0:
            current = current[LEFT][LEFT][LEFT][LEFT][LEFT][LEFT][LEFT]
            scores[(n - 1) % players] += n + current[0]

            current[LEFT][RIGHT] = current[RIGHT]
            current[RIGHT][LEFT] = current[LEFT]
            current = current[RIGHT]
        else:
            current = [n, current[RIGHT], current[RIGHT][RIGHT]]
            current[RIGHT][LEFT] = current[LEFT][RIGHT] = current

    return scores
