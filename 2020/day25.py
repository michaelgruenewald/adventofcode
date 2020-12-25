#!/usr/bin/env python

card_pk, door_pk = 5764801, 17807724
card_pk, door_pk = 8335663, 8614349

def transform(subject, loopsize):
    v = 1
    for i in range(loopsize):
        v = (v * subject) % 20201227
    return v


def all_transforms(subject):
    v = 1
    while True:
        yield v
        v = (v * subject) % 20201227



card_loop = next(i for i, v in enumerate(all_transforms(7)) if v == card_pk)
door_loop = next(i for i, v in enumerate(all_transforms(7)) if v == door_pk)

card_key = transform(door_pk, card_loop)
door_key = transform(card_pk, door_loop)

print(card_key, door_key)
