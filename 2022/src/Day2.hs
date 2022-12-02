module Day2 (part1, part2) where

import Data.Bifunctor

data First = A | B | C deriving (Read)

data Second = X | Y | Z deriving (Read)

data Shape = Rock | Paper | Scissors

data Outcome = Lose | Draw | Win

outcome :: Shape -> Shape -> Outcome
outcome Paper Rock = Lose
outcome Paper Scissors = Win
outcome Rock Paper = Win
outcome Rock Scissors = Lose
outcome Scissors Paper = Lose
outcome Scissors Rock = Win
outcome _ _ = Draw

outcomeScore :: Outcome -> Int
outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win = 6

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

requiredResponse :: Shape -> Outcome -> Shape
requiredResponse Rock Lose = Scissors
requiredResponse Rock Win = Paper
requiredResponse Paper Lose = Rock
requiredResponse Paper Win = Scissors
requiredResponse Scissors Lose = Paper
requiredResponse Scissors Win = Rock
requiredResponse x Draw = x

part1 :: String -> Int
part1 s = do
  let parsed = map (bimap (convertFirst . read) (convertSecond . read) . break (== ' ')) $ lines s
        where
          convertFirst x = case x of
            A -> Rock
            B -> Paper
            C -> Scissors
          convertSecond x = case x of
            X -> Rock
            Y -> Paper
            Z -> Scissors
  sum $ map (\(opponent, response) -> shapeScore response + outcomeScore (outcome opponent response)) parsed

part2 :: String -> Int
part2 s = do
  let parsed = map (bimap (convertFirst . read) (convertSecond . read) . break (== ' ')) $ lines s
        where
          convertFirst x = case x of
            A -> Rock
            B -> Paper
            C -> Scissors
          convertSecond x = case x of
            X -> Lose
            Y -> Draw
            Z -> Win
  sum $ map (\(opponent, desiredOutcome) -> shapeScore (requiredResponse opponent desiredOutcome) + outcomeScore desiredOutcome) parsed
