module Main (main) where

import Data.List ( sort )

main :: IO ()
main = do
  contents <- readFile "input1.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 contents = do
  let elves = partitionBy ("" ==) (lines contents)
  let caloriesByElf = map (sum . map read) elves
  maximum caloriesByElf

part2 :: String -> Int
part2 contents = do
  let elves = partitionBy ("" ==) (lines contents)
  let caloriesByElf = map (sum . map read) elves
  sum $ take 3 $ reverse $ sort caloriesByElf

partitionBy :: (a -> Bool) -> [a] -> [[a]]
partitionBy _ [] = []
partitionBy cond l = do
  let (left, right) = break cond l
  left : partitionBy cond (drop 1 right)
