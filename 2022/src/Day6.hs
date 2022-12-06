module Day6 (main) where

import Data.List (nub)

firstUnique :: Int -> String -> Int
firstUnique n s
  | length (nub (take n s)) == n = n
  | otherwise = firstUnique n (tail s) + 1

part1 :: String -> Int
part1 = firstUnique 4

part2 :: String -> Int
part2 = firstUnique 14

main :: IO ()
main = do
  contents <- readFile "src/input6.txt"
  print $ part1 contents
  print $ part2 contents
