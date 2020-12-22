#!/usr/bin/env python
from copy import deepcopy

problem = """\
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
"""

problem = open("input22.txt").read()
blobs = problem.split("\n\n")
initial = [[int(x) for x in blob.splitlines()[1:]] for blob in blobs]

# part 1

players = deepcopy(initial)

while all(players):
    cards = [player.pop(0) for player in players]
    lost, won = sorted(cards)
    winner = cards.index(won)
    players[winner].extend((won, lost))

(winner,) = (player for player in players if player)

print(sum((p + 1) * v for p, v in enumerate(winner[::-1])))


# part 2


def game(players):
    rounds = []
    while all(players):
        if players in rounds:
            return 0, None
        else:
            rounds.append(deepcopy(players))
        cards = [player.pop(0) for player in players]
        if all(card <= len(player) for card, player in zip(cards, players)):
            winner, _ = game([player[:card] for card, player in zip(cards, players)])
            won = cards[winner]
            lost = cards[not winner]
        else:
            lost, won = sorted(cards)
            winner = cards.index(won)
        players[winner].extend((won, lost))
    (winner,) = (i for i, player in enumerate(players) if player)
    return winner, players


winner, players = game(deepcopy(initial))

print(sum((p + 1) * v for p, v in enumerate(players[winner][::-1])))
