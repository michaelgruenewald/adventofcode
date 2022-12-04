module Day4 (part1, part2) where

import Data.List (intersect, union)

parseRange :: [Char] -> [Int]
parseRange t = enumFromTo (read l) (read (tail r)) where (l, r) = break (== '-') t

parse :: String -> [([Int], [Int])]
parse =
  let parseline line = (parseRange l, parseRange (tail r)) where (l, r) = break (== ',') line
   in map parseline . lines

part1 :: String -> Int
part1 = length . filter (\(a, b) -> length (a `union` b) == max (length a) (length b)) . parse

part2 :: String -> Int
part2 = length . filter (\(a, b) -> not (null (a `intersect` b))) . parse
