module Day05 (main) where

import Data.Bifunctor (bimap, second)
import Data.Char (digitToInt, isAlpha, isDigit)
import Data.List (transpose)
import Data.List.Extra (breakOn)
import Data.Map (Map, adjust, elems, fromList, (!))
import Paths_aoc (getDataFileName)

breakOn' :: Eq a => [a] -> [a] -> ([a], [a])
breakOn' s l = second (drop $ length s) $ breakOn s l

part1 :: String -> String
part1 = map head . elems . uncurry (foldl apply) . parse
  where
    apply state (amount, from, to) = (pop . push) state
      where
        pop = adjust (drop amount) from
        push = adjust ((reverse $ take amount (state ! from)) ++) to

part2 :: String -> String
part2 = map head . elems . uncurry (foldl apply) . parse
  where
    apply state (amount, from, to) = (pop . push) state
      where
        pop = adjust (drop amount) from
        push = adjust (take amount (state ! from) ++) to

parse :: String -> (Map Int String, [(Int, Int, Int)])
parse =
  let parseStart = fromList . map (\(x : xs) -> (digitToInt x, reverse . filter isAlpha $ xs)) . filter (isDigit . head) . transpose . reverse . lines
      parseProcedure = map ((\["move", amount, "from", from, "to", to] -> (read amount, read from, read to)) . words) . lines
   in bimap parseStart parseProcedure . breakOn' "\n\n"

main :: IO ()
main = do
  contents <- getDataFileName "input05.txt" >>= readFile
  print $ part1 contents
  print $ part2 contents
