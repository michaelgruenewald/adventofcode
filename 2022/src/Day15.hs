module Day15 (main) where

import Data.List (nub)
import qualified Data.Set as S
import Paths_aoc (getDataFileName)
import qualified Text.Parsec as P

type Pos = (Int, Int)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

turn :: Pos -> Int -> Pos
turn p 0 = p
turn p n = (y, -x) where (x, y) = turn p (n - 1)

parser :: Monad i => P.ParsecT String u i [(Pos, Pos)]
parser =
  let numParser = do
        sign <- P.option "" (P.string "-")
        digits <- P.many1 P.digit
        return (read (sign ++ digits))
      posParser = do
        x <- P.string "x=" >> numParser
        y <- P.string ", y=" >> numParser
        return (x, y)
      lineParser = do
        P.string "Sensor at "
        sensor <- posParser
        P.string ": closest beacon is at "
        beacon <- posParser
        P.newline
        return (sensor, beacon)
   in P.many1 lineParser <* P.eof

parse :: String -> [(Pos, Pos)]
parse = either (error . show) id . P.parse parser "input15.txt"

coveredLine :: Int -> (Pos, Pos) -> S.Set Pos
coveredLine y (sensor, beacon) =
  let r = dist sensor beacon - abs (snd sensor - y)
   in S.delete beacon $ S.fromList [(fst sensor + rx, y) | r >= 0, rx <- [-r .. r]]

part1 :: String -> Int
part1 = length . foldl S.union S.empty . map (coveredLine 2000000) . parse

data Diamond = Diamond Pos Int deriving (Show)

-- | Does the diamond cover a position?
covers :: Diamond -> Pos -> Bool
covers (Diamond center distance) pos = dist center pos <= distance

-- | Returns all the positions that are just outside the edges of the diamond
edges :: Diamond -> [(Int, Int)]
edges (Diamond center distance) = [center @+ turn (e, -distance - 1 + e) t | e <- [0 .. distance], t <- [0 .. 3]]

only :: [a] -> a
only [x] = x
only _ = error "not one element"

part2 :: String -> Int
part2 s =
  let parsed = parse s
      diamonds = map (\(sensor, beacon) -> Diamond sensor (dist sensor beacon)) parsed
      nearby (x, y) = x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000
      uncovered pos = not $ any (`covers` pos) diamonds
      frequency (x, y) = x * 4000000 + y
   in frequency $ only $ nub $ filter uncovered $ filter nearby $ concatMap edges diamonds

main :: IO ()
main = do
  contents <- getDataFileName "input15.txt" >>= readFile
  print $ part1 contents
  print $ part2 contents
