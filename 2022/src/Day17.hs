{-# LANGUAGE LambdaCase #-}

module Day17 (main) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Debug.Trace (traceShowId)

type Pos = (Int, Int)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

newtype Rock = Rock (S.Set Pos) deriving (Show)

hitsWall :: Rock -> Bool
hitsWall (Rock pos) = any (\(x, y) -> x < 0 || x > 6) pos

hits :: S.Set Pos -> Rock -> Bool
hits blocked (Rock pos) = not $ null $ blocked `S.intersection` pos

move :: Rock -> Pos -> Rock
move (Rock pos) dir = Rock (S.map (@+ dir) pos)

rockShapes :: [Rock]
rockShapes =
  [ Rock (S.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]),
    Rock (S.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]),
    Rock (S.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]),
    Rock (S.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]),
    Rock (S.fromList [(0, 0), (1, 0), (0, 1), (1, 1)])
  ]

parse :: String -> [Pos]
parse =
  mapMaybe
    ( \case
        '<' -> Just (-1, 0)
        '>' -> Just (1, 0)
        _ -> Nothing
    )

fall :: S.Set Pos -> Rock -> [Pos] -> (Rock, [Pos])
fall blocked rock movements =
  let horizontal = if hitsWall rock' || hits blocked rock' then rock else rock' where rock' = move rock (head movements)
      vertical = move horizontal (0, -1)
   in if hits blocked vertical
        then (horizontal, tail movements)
        else fall blocked vertical (tail movements)

run :: Int -> String -> Int
run n s =
  let initialBlocked = S.fromList [(x, 0) | x <- [0 .. 6]]
      initialMovements = cycle (parse s)
      rocks = cycle rockShapes
      step (height, blocked, movements) shape =
        let startingBlock = move shape (2, 4)
            (Rock restingPos, remainingMovements) = fall blocked startingBlock movements
            newBlocked = blocked `S.union` restingPos
            gain = maximum (map snd (S.elems newBlocked))
            adjustedBlocked = S.map (@+ (0, -gain)) newBlocked
         in (height + gain, adjustedBlocked, remainingMovements)
   in (\(h, _, _) -> h) $ foldl step (0, initialBlocked, initialMovements) $ take n rocks

part1 :: String -> Int
part1 = run 2022

part2 :: String -> Int
part2 = run 2023

main :: IO ()
main = do
  contents <- readFile "src/input17.txt"
  print $ part1 contents
  print $ part2 contents
