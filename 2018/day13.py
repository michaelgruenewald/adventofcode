#!/usr/bin/env pytohn
import itertools

data = r"""/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/
"""

data = r"""/>-<\
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/"""

data = open("input13.txt").read()

UP = (-1, 0)
DOWN = (1, 0)
LEFT = (0, -1)
RIGHT = (0, 1)

CART_SYMBOLS = {
    ">": (RIGHT, "-"),
    "<": (LEFT, "-"),
    "^": (UP, "|"),
    "v": (DOWN, "|"),
}


class Cart:
    def __init__(self, row, col, symbol):
        self.row = row
        self.col = col
        self.direction = CART_SYMBOLS[symbol][0]
        self.turn_state = 0

    def tick(self):
        self.row, self.col = self.row + self.direction[0], self.col + self.direction[1]
        track = tracks[self.row, self.col]
        if self.direction[0] == 0 and track == "-":
            pass
        elif self.direction[1] == 0 and track == "|":
            pass
        elif track == "+":
            if self.turn_state == 0:
                self.direction = -self.direction[1], self.direction[0]  # left
            elif self.turn_state == 2:
                self.direction = self.direction[1], -self.direction[0]  # right
            self.turn_state = (self.turn_state + 1) % 3
        elif (
            self.direction == RIGHT
            and track == "\\"
            or self.direction == LEFT
            and track == "/"
        ):
            self.direction = DOWN
        elif (
            self.direction == RIGHT
            and track == "/"
            or self.direction == LEFT
            and track == "\\"
        ):
            self.direction = UP
        elif (
            self.direction == DOWN
            and track == "\\"
            or self.direction == UP
            and track == "/"
        ):
            self.direction = RIGHT
        elif (
            self.direction == DOWN
            and track == "/"
            or self.direction == UP
            and track == "\\"
        ):
            self.direction = LEFT
        else:
            assert False

    def __repr__(self):
        return f"<Cart {self.col},{self.row} d={self.direction}>"


tracks = {}
carts = []

row, col = 0, 0
for c in data:
    if c == "\n":
        row, col = row + 1, 0
        continue

    if c in CART_SYMBOLS:
        carts.append(Cart(row, col, c))
        c = CART_SYMBOLS[c][1]

    if c in r"\/-|+":
        tracks[row, col] = c
    elif c == " ":
        pass
    else:
        assert False

    col += 1

while len(carts) > 1:
    for cart in sorted(carts, key=lambda cart: (cart.row, cart.col)):
        cart.tick()
        crashing = [c for c in carts if c is not cart and c.row == cart.row and c.col == cart.col]
        if crashing:
            for c in crashing:
                carts.remove(c)
            carts.remove(cart)

print(carts)
