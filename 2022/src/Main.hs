module Main (main) where

import Data.List (sort)

main :: IO ()
main = do
  contents <- readFile "input1.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 = maximum . map sum . parse

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map sum . parse

parse :: String -> [[Int]]
parse s = map (map read) $ partitionBy null $ lines s

partitionBy :: (a -> Bool) -> [a] -> [[a]]
partitionBy _ [] = []
partitionBy cond l = do
  let (left, right) = break cond l
  left : partitionBy cond (drop 1 right)
