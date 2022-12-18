module Day18 (main) where

import Data.List.Extra (splitOn)
import qualified Data.Set as S
import Paths_aoc (getDataFileName)

type Pos = (Int, Int, Int)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

parse :: String -> [Pos]
parse = map ((\[x, y, z] -> (read x, read y, read z)) . splitOn ",") . lines

sides :: [Pos]
sides = [(0, 0, -1), (0, 0, 1), (0, -1, 0), (0, 1, 0), (-1, 0, 0), (1, 0, 0)]

part1 :: String -> Int
part1 s =
  let cubes = S.fromList $ parse s
   in sum $ map (\cube -> length $ filter (\side -> (cube @+ side) `notElem` cubes) sides) $ S.elems cubes

fill :: (Pos -> Bool) -> S.Set Pos -> S.Set Pos -> S.Set Pos
fill cond filled lookAt
  | null lookAt = filled
  | otherwise =
      let consider = S.fromList $ concatMap (\cube -> map (cube @+) sides) lookAt
          new = S.filter (\cube -> cube `notElem` filled && cond cube) consider
       in fill cond (filled `S.union` new) new

part2 :: String -> Int
part2 s =
  let cubes = S.fromList $ parse s
      (xMin, yMin, zMin) = foldl1 (\(mx, my, mz) (x, y, z) -> (min mx x, min my y, min mz z)) cubes
      (xMax, yMax, zMax) = foldl1 (\(mx, my, mz) (x, y, z) -> (max mx x, max my y, max mz z)) cubes
      inBounds (x, y, z) = x >= xMin - 1 && x <= xMax + 1 && y >= yMin - 1 && y <= yMax + 1 && z >= zMin - 1 && z <= zMax + 1
      air = fill (\cube -> cube `notElem` cubes && inBounds cube) S.empty (S.singleton (0, 0, 0))
   in sum $ map (\cube -> length $ filter (\side -> (cube @+ side) `elem` air) sides) $ S.elems cubes

main :: IO ()
main = do
  contents <- getDataFileName "input18.txt" >>= readFile
  print $ part1 contents
  print $ part2 contents
