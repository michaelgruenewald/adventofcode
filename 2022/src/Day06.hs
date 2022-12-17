module Day06 (main) where

import Data.List (nub)
import Paths_aoc (getDataFileName)

firstUnique :: Int -> String -> Int
firstUnique n s
  | take n s == nub (take n s) = n
  | otherwise = firstUnique n (tail s) + 1

part1 :: String -> Int
part1 = firstUnique 4

part2 :: String -> Int
part2 = firstUnique 14

main :: IO ()
main = do
  contents <- getDataFileName "input06.txt" >>= readFile
  print $ part1 contents
  print $ part2 contents
