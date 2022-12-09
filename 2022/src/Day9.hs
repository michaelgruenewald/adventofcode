module Day9 (main) where

import Data.Bifunctor (bimap)
import Data.Set (Set, insert, singleton)
import Debug.Trace (traceShowId)

type Pos = (Int, Int)

(@-) :: Pos -> Pos -> Pos
(@-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Dir = D | U | L | R deriving (Read, Show)

move :: Dir -> Pos -> Pos
move D = (@+ (0, -1))
move U = (@+ (0, 1))
move L = (@+ (-1, 0))
move R = (@+ (1, 0))

data State = State {knotsAt :: [Pos], seen :: Set Pos} deriving (Show)

parse :: String -> [(Dir, Int)]
parse = map (bimap read read . break (== ' ')) . lines

unroll :: [(Dir, Int)] -> [Dir]
unroll = concatMap (\(dir, cnt) -> replicate cnt dir)

run :: Int -> String -> Int
run n = length . seen . foldl (\state i -> updateSeen $ updateKnots state i) initial . unroll . parse
  where
    initial = State (replicate n (0, 0)) (singleton (0, 0))
    updateKnots st i = st {knotsAt = scanl moveKnot (move i $ head $ knotsAt st) (tail $ knotsAt st)}
      where
        moveKnot follow pos = if abs xd == 2 || abs yd == 2 then pos @- (signum xd, signum yd) else pos where (xd, yd) = pos @- follow
    updateSeen st = st {seen = insert (last $ knotsAt st) (seen st)}

part1 :: String -> Int
part1 = run 2

part2 :: String -> Int
part2 = run 10

main :: IO ()
main = do
  contents <- readFile "src/input9.txt"
  print $ part1 contents
  print $ part2 contents
