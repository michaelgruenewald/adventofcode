module Day09 (main) where

import Data.Bifunctor (bimap)
import Data.List (nub)

type Pos = (Int, Int)

(@-) :: Pos -> Pos -> Pos
(@-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Dir = D | U | L | R deriving (Read, Show)

move :: Pos -> Dir -> Pos
move p D = p @+ (0, -1)
move p U = p @+ (0, 1)
move p L = p @+ (-1, 0)
move p R = p @+ (1, 0)

parse :: String -> [(Dir, Int)]
parse = map (bimap read read . break (== ' ')) . lines

unroll :: [(Dir, Int)] -> [Dir]
unroll = concatMap (\(dir, cnt) -> replicate cnt dir)

follow :: Pos -> Pos -> Pos
follow self target = if abs xd == 2 || abs yd == 2 then self @- (signum xd, signum yd) else self where (xd, yd) = self @- target

locations :: Int -> [Dir] -> [Pos]
locations 0 = scanl move (0, 0)
locations i = scanl follow (0, 0) . locations (i - 1)

run :: Int -> String -> Int
run n s = length $ nub $ locations n $ (unroll . parse) s

part1 :: String -> Int
part1 = run 1

part2 :: String -> Int
part2 = run 9

main :: IO ()
main = do
  contents <- readFile "src/input09.txt"
  print $ part1 contents
  print $ part2 contents
