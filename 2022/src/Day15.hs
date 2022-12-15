module Day15 (main) where

import Control.Lens (none)
import Data.Functor (($>))
import qualified Data.Functor.Identity
import Data.List (nub)
import qualified Data.Set as S
import qualified Text.Parsec as P

type Pos = (Int, Int)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

parser :: P.ParsecT String u Data.Functor.Identity.Identity [(Pos, Pos)]
parser =
  let numParser = do
        minus <- P.option 1 (P.char '-' $> (-1))
        digits <- P.many1 P.digit
        return (minus * read digits)
      posParser = do
        x <- P.string "x=" >> numParser
        y <- P.string ", y=" >> numParser
        return (x, y)
      lineParser = do
        sensor <- P.string "Sensor at " >> posParser
        beacon <- P.string ": closest beacon is at " >> posParser
        P.newline
        return (sensor, beacon)
   in P.many1 lineParser <* P.eof

parse :: String -> [(Pos, Pos)]
parse = either (error . show) id . P.parse parser "input15.txt"

coveredLine :: Int -> (Pos, Pos) -> S.Set Pos
coveredLine y (sensor, beacon) = case dist sensor beacon - abs (snd sensor - y) of
  r | r <= 0 -> S.empty
  r -> S.delete beacon $ S.fromList [(fst sensor + rx, y) | rx <- [-r .. r]]

part1 :: String -> Int
part1 = length . foldl S.union S.empty . map (coveredLine 2000000) . parse

data Diamond = Diamond Pos Int deriving (Show)

-- | Does the diamond cover a position?
covers :: Diamond -> Pos -> Bool
covers (Diamond center distance) pos = dist center pos <= distance

-- | Returns all the positions that are just outside the edges of the diamond
edges :: Diamond -> [(Int, Int)]
edges (Diamond (cx, cy) d) = concat [[(cx - d - 1 + r, cy + r), (cx + d + 1 - r, cy - r), (cx + r, cy - d - 1 + r), (cx - r, cy + d + 1 - r)] | r <- [0 .. d]]

only :: [a] -> a
only [x] = x
only _ = error "not one element"

part2 :: String -> Int
part2 s =
  let parsed = parse s
      diamonds = map (\(sensor, beacon) -> Diamond sensor (dist sensor beacon)) parsed
      interesting = filter (\(x, y) -> x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000) $ concatMap edges diamonds
   in (\(x, y) -> x * 4000000 + y) $ only $ nub $ filter (\pos -> none (`covers` pos) diamonds) interesting

main :: IO ()
main = do
  contents <- readFile "src/input15.txt"
  print $ part1 contents
  print $ part2 contents
