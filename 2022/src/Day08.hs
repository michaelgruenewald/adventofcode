module Day08 (main) where

import Data.Char (digitToInt)
import Data.Map (Map, filterWithKey, fromList, mapWithKey, (!?))
import Data.Maybe (fromJust, isJust)

parse :: String -> Map (Int, Int) Int
parse s = fromList [((row, col), digitToInt h) | (hs, row) <- zip (lines s) [0 ..], (h, col) <- zip hs [0 ..]]

dirs :: (Int, Int) -> Map (Int, Int) Int -> [[Int]]
dirs (row, col) m =
  [ fuse [m !? (row, c) | c <- [col + 1 ..]],
    fuse [m !? (row, c) | c <- [col - 1, col - 2 ..]],
    fuse [m !? (r, col) | r <- [row + 1 ..]],
    fuse [m !? (r, col) | r <- [row - 1, row - 2 ..]]
  ]

fuse :: [Maybe b] -> [b]
fuse = map fromJust . takeWhile isJust

part1 :: String -> Int
part1 s = length $ filterWithKey (\pos height -> any (all (< height)) (dirs pos m)) m where m = parse s

part2 :: String -> Int
part2 s = maximum $ mapWithKey (\pos height -> product $ map (length . takeUntil (>= height)) (dirs pos m)) m where m = parse s

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

main :: IO ()
main = do
  contents <- readFile "src/input08.txt"
  print $ part1 contents
  print $ part2 contents
