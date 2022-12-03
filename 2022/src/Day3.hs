module Day3 (part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.List (intersect, nub)

priority :: Char -> Int
priority c
  | isAsciiUpper c = ord c - ord 'A' + 27
  | isAsciiLower c = ord c - ord 'a' + 1
  | otherwise = error "invalid item type"

groupOf :: Int -> [a] -> [[a]]
groupOf _ [] = []
groupOf n a = let (l, r) = splitAt n a in l : groupOf n r

part1 :: String -> Int
part1 = sum . map (priority . head . uncurry intersect . bimap nub nub . (\line -> splitAt (div (length line) 2) line)) . lines

part2 :: String -> Int
part2 = sum . map (priority . head . foldl1 intersect . map nub) . groupOf 3 . lines
