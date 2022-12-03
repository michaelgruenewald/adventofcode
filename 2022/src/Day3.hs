module Day3 (part1, part2) where

import Data.Bifunctor
import Data.Char
import qualified Data.Set as S

priority :: Char -> Int
priority c
  | isAsciiUpper c = ord c - ord 'A' + 27
  | isAsciiLower c = ord c - ord 'a' + 1
  | otherwise = error "invalid item type"

groupOf :: Int -> [a] -> [[a]]
groupOf _ [] = []
groupOf n a = let (l, r) = splitAt n a in l : groupOf n r

part1 :: String -> Int
part1 = sum . map (sum . S.map priority . uncurry S.intersection . bimap S.fromList S.fromList . (\line -> splitAt (div (length line) 2) line)) . lines

part2 :: String -> Int
part2 = sum . map (sum . S.map priority . foldl1 S.intersection . map S.fromList) . groupOf 3 . lines
