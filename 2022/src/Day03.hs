module Day03 (main) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.List (intersect, nub)

priority :: Char -> Int
priority c
  | isAsciiUpper c = ord c - ord 'A' + 27
  | isAsciiLower c = ord c - ord 'a' + 1
  | otherwise = error "invalid item type"

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n list = l : chunks n r where (l, r) = splitAt n list

halves :: [a] -> ([a], [a])
halves list = splitAt (div (length list) 2) list

only :: [a] -> a
only [x] = x
only _ = error "not one element"

part1 :: String -> Int
part1 = sum . map (priority . only . uncurry intersect . bimap nub nub . halves) . lines

part2 :: String -> Int
part2 = sum . map (priority . only . foldl1 intersect . map nub) . chunks 3 . lines

main :: IO ()
main = do
  contents <- readFile "src/input03.txt"
  print $ part1 contents
  print $ part2 contents
