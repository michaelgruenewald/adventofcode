module Day2 (main) where

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
part1 =
  let convertFirst x = case read x of A -> Rock; B -> Paper; C -> Scissors
      convertSecond x = case read x of X -> Rock; Y -> Paper; Z -> Scissors
      score (opponent, response) = shapeScore response + outcomeScore (outcome opponent response)
   in sum . map (score . bimap convertFirst convertSecond . break (== ' ')) . lines

part2 :: String -> Int
part2 =
  let convertFirst x = case read x of A -> Rock; B -> Paper; C -> Scissors
      convertSecond x = case read x of X -> Lose; Y -> Draw; Z -> Win
      score (opponent, desiredOutcome) = shapeScore (requiredResponse opponent desiredOutcome) + outcomeScore desiredOutcome
   in sum . map (score . bimap convertFirst convertSecond . break (== ' ')) . lines

main :: IO ()
main = do
  contents <- readFile "src/input2.txt"
  print $ part1 contents
  print $ part2 contents
