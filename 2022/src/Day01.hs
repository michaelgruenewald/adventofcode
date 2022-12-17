module Day01 (main) where

import Data.List (sort)
import Data.List.Extra (split)
import Paths_aoc (getDataFileName)

part1 :: String -> Int
part1 = maximum . map sum . parse

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map sum . parse

main :: IO ()
main = do
  contents <- getDataFileName "input01.txt" >>= readFile
  print $ part1 contents
  print $ part2 contents

parse :: String -> [[Int]]
parse = map (map read) . split null . lines
