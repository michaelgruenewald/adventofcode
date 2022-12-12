module Day12 (main) where

import Control.Lens
import Data.Char (digitToInt)
import Data.Foldable (find, minimumBy)
import qualified Data.Foldable as M
import Data.Function (on)
import Data.Map (member, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Tuple.Extra (snd3)
import Debug.Trace (traceShow, traceShowId)

type Pos = (Int, Int)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

adjacent :: Pos -> [Pos]
adjacent pos = map (pos @+) [(0, -1), (0, 1), (-1, 0), (1, 0)]

parse :: String -> [(Pos, Char)]
parse s = [((x, y), h) | (hs, x) <- zip (lines s) [0 ..], (h, y) <- zip hs [0 ..]]

fixHeights :: Char -> Char
fixHeights c = case c of 'S' -> 'a'; 'E' -> 'z'; other -> other

run :: (Char -> Bool) -> String -> Int
run t s =
  let parsed = parse s
      plan = M.map fixHeights $ M.fromList parsed
      start = fst . fromJust $ find (\(_, h) -> h == 'E') parsed
      destinations = map fst $ filter (\(_, h) -> t h) parsed
      initial = (M.fromList $ map (\pos -> (pos, if pos == start then Just 0 else Nothing)) (M.keys plan))
      step state =
        let (next, Just cost) = minimumBy (compare `on` snd) $ M.assocs $ M.filter isJust state
            reachable = filter (\adj -> maybe False (>= pred (plan ! next)) (plan !? adj)) (adjacent next)
         in M.delete next $ foldl (flip (M.adjust (Just . maybe (cost + 1) (min (cost + 1))))) state reachable
      done state = any (\d -> isJust (state ! d)) destinations
   in fromJust $ head $ M.elems $ M.filterWithKey (\k v -> k `elem` destinations && isJust v) $ until done step initial

part1 :: String -> Int
part1 = run (== 'S')

part2 :: String -> Int
part2 = run (\c -> c == 'S' || c == 'a')

main :: IO ()
main = do
  contents <- readFile "src/input12.txt"
  print $ part1 contents
  print $ part2 contents
