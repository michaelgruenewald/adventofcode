#!/usr/bin/env pytohn

data = r"""/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
"""

CART_SYMBOLS = {
    ">": ((0, 1), "-"),
    "<": ((0, -1), "-"),
    "^": ((-1, 0), "|"),
    "v": ((1, 0), "|"),
}

tracks = {}
carts = []

row, col = 0, 0
for c in data:
    if c == "\n":
        row, col = row + 1, 0
        continue
    else:
        col += 1

    if c in CART_SYMBOLS:
        cart, c = CART_SYMBOLS[c]
        carts.append((row, col, cart[0], cart[1]))

    if c in r"\/-|+":
        tracks[(row, col)] = c
    elif c == " ":
        pass
    else:
        assert False

while True:
    carts.sort()
    for i in range(len(carts)):
        row, col, drow, dcol = carts.pop(0)
        row += drow
        col += dcol

        carts.append(cart)
    print carts
    break
