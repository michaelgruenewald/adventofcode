module Day1 (main) where

import Data.List (sort)

part1 :: String -> Int
part1 = maximum . map sum . parse

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map sum . parse

main :: IO ()
main = do
  contents <- readFile "src/input1.txt"
  print $ part1 contents
  print $ part2 contents

parse :: String -> [[Int]]
parse = map (map read) . splitBy null . lines

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy cond l =
  let (left, right) = break cond l
   in left : splitBy cond (drop 1 right)
