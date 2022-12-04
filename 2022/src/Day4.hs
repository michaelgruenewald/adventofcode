module Day4 (main) where

import Data.List (intersect, isSubsequenceOf)

parseRange :: [Char] -> [Int]
parseRange t = enumFromTo (read l) (read (tail r)) where (l, r) = break (== '-') t

parse :: String -> [([Int], [Int])]
parse =
  let parseline line = (parseRange l, parseRange (tail r)) where (l, r) = break (== ',') line
   in map parseline . lines

part1 :: String -> Int
part1 = length . filter (\(a, b) -> isSubsequenceOf a b || isSubsequenceOf b a) . parse

part2 :: String -> Int
part2 = length . filter (not . null . uncurry intersect) . parse

main :: IO ()
main = do
  contents <- readFile "src/input4.txt"
  print $ part1 contents
  print $ part2 contents
