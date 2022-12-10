module Day04 (main) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (intersect, isSubsequenceOf)

parseRange :: [Char] -> [Int]
parseRange = uncurry enumFromTo . bimap read (read . tail) . break (== '-')

parse :: String -> [([Int], [Int])]
parse = map (bimap parseRange (parseRange . tail) . break (== ',')) . lines

part1 :: String -> Int
part1 = length . filter (\(a, b) -> isSubsequenceOf a b || isSubsequenceOf b a) . parse

part2 :: String -> Int
part2 = length . filter (not . null . uncurry intersect) . parse

main :: IO ()
main = do
  contents <- readFile "src/input04.txt"
  print $ part1 contents
  print $ part2 contents
