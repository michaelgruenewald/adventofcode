{-# LANGUAGE TupleSections #-}

module Day16 (main) where

import Data.List (delete, insert, partition)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Paths_aoc (getDataFileName)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P

parser :: Monad i => P.ParsecT String u i [(String, Int, [String])]
parser =
  let numParser = do
        digits <- P.many1 P.digit
        return (read digits)
      nameParser = P.many1 P.upper
      lineParser = do
        P.string "Valve "
        valveId <- nameParser
        P.string " has flow rate="
        flowRate <- numParser
        P.try (P.string "; tunnel leads to valve ") <|> P.try (P.string "; tunnels lead to valves ")
        tunnelsTo <- P.sepBy1 nameParser (P.string ", ")
        P.newline
        return (valveId, flowRate, tunnelsTo)
   in P.many1 lineParser <* P.eof

parse :: String -> [(String, Int, [String])]
parse = either (error . show) id . P.parse parser "input16.txt"

run :: Int -> [String] -> String -> Int
run totalTime startPositions s =
  let valves = parse s
      valveIds = map (\(v, _, _) -> v) valves
      neighbors = M.fromList $ map (\(v, _, vs) -> (v, vs)) valves
      flowrates = M.fromList $ map (\(v, f, _) -> (v, f)) valves
      useful = mapMaybe (\v -> if flowrates ! v /= 0 then Just v else Nothing) valveIds

      distances' from destinations =
        let (adjacent, remote) = partition (`elem` neighbors ! from) destinations
         in M.fromList (map (,1) adjacent) `M.union` M.map (+ 1) (foldl (M.unionWith min) M.empty $ map (`distances'` remote) adjacent)
      distances = M.fromList $ map (\from -> (from, distances' from (delete from valveIds))) valveIds

      mostPressure [] _ = 0
      mostPressure ((time, position) : rest) closedValves =
        maximum $
          mostPressure rest closedValves -- slacking off
            : [ addedPressure + mostPressure (insert (newTime, nextValve) rest) (delete nextValve closedValves)
                | nextValve <- closedValves,
                  let newTime = time + distances ! position ! nextValve + 1,
                  newTime <= totalTime,
                  let addedPressure = max 0 (totalTime - newTime) * (flowrates ! nextValve)
              ]
   in mostPressure (map (0,) startPositions) useful

part1 :: String -> Int
part1 = run 30 ["AA"]

part2 :: String -> Int
part2 = run 26 ["AA", "AA"]

main :: IO ()
main = do
  contents <- getDataFileName "input16.txt" >>= readFile
  print $ part1 contents
  print $ part2 contents
