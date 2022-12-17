module Day12 (main) where

import Data.Foldable (find, minimumBy)
import Data.Function (on)
import Data.Map ((!), (!?))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Paths_aoc (getDataFileName)

type Pos = (Int, Int)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

adjacent :: Pos -> [Pos]
adjacent pos = map (pos @+) [(0, -1), (0, 1), (-1, 0), (1, 0)]

parse :: String -> [(Pos, Char)]
parse s = [((x, y), h) | (hs, x) <- zip (lines s) [0 ..], (h, y) <- zip hs [0 ..]]

fixHeights :: Char -> Char
fixHeights 'S' = 'a'
fixHeights 'E' = 'z'
fixHeights x = x

run :: (Char -> Bool) -> String -> Int
run t s =
  let parsed = parse s
      plan = M.map fixHeights $ M.fromList parsed
      start = fst . fromJust $ find ((==) 'E' . snd) parsed
      destinations = map fst $ filter (t . snd) parsed
      initial = (M.fromList $ map (\pos -> (pos, if pos == start then Just 0 else Nothing)) (M.keys plan))
      step state =
        let (next, Just cost) = minimumBy (compare `on` snd) $ M.assocs $ M.filter isJust state
            reachable = filter (\adj -> maybe False (>= pred (plan ! next)) (plan !? adj)) (adjacent next)
         in M.delete next $ foldl (flip $ M.adjust (Just . maybe (cost + 1) (min (cost + 1)))) state reachable
      done state = any (isJust . (state !)) destinations
   in fromJust $ head $ M.elems $ M.filterWithKey (\k v -> k `elem` destinations && isJust v) $ until done step initial

part1 :: String -> Int
part1 = run (== 'S')

part2 :: String -> Int
part2 = run (`elem` ['S', 'a'])

main :: IO ()
main = do
  contents <- getDataFileName "input12.txt" >>= readFile
  print $ part1 contents
  print $ part2 contents
