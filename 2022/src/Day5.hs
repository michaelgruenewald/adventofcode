module Day5 (main) where

import Data.Bifunctor (bimap, second)
import Data.Char (digitToInt, isAlpha, isDigit)
import Data.List (transpose)
import Data.List.Extra (breakOn, splitOn)
import Data.Map ((!))
import qualified Data.Map as M

breakOn' :: Eq a => [a] -> [a] -> ([a], [a])
breakOn' s l = second (drop $ length s) $ breakOn s l

part1 :: String -> String
part1 = map head . M.elems . uncurry (foldl apply) . parse
  where
    apply state (amount, from, to) = pop $ push state
      where
        pop = M.adjust (drop amount) from
        push = M.adjust ((reverse $ take amount (state ! from)) ++) to

part2 :: String -> String
part2 = map head . M.elems . uncurry (foldl apply) . parse
  where
    apply state (amount, from, to) = pop $ push state
      where
        pop = M.adjust (drop amount) from
        push = M.adjust (take amount (state ! from) ++) to

parse :: String -> (M.Map Int String, [(Int, Int, Int)])
parse =
  let parseStart = M.fromList . map (\(x : xs) -> (digitToInt x, reverse . filter isAlpha $ xs)) . filter (isDigit . head) . transpose . reverse . lines
      parseProcedure = map ((\["move", amount, "from", from, "to", to] -> (read amount, read from, read to)) . splitOn " ") . lines
   in bimap parseStart parseProcedure . breakOn' "\n\n"

main :: IO ()
main = do
  contents <- readFile "src/input5.txt"
  print $ part1 contents
  print $ part2 contents
