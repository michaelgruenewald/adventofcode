{-# LANGUAGE TupleSections #-}

module Day14 (main) where

import Control.Monad (liftM2)
import Data.Functor ((<&>))
import qualified Data.Functor.Identity
import Data.List (find)
import qualified Data.Set as S
import qualified Text.Parsec as P

type Pos = (Int, Int)

(@+) :: Pos -> Pos -> Pos
(@+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

numParser :: P.ParsecT String u Data.Functor.Identity.Identity Int
numParser = P.many1 P.digit <&> read

parser :: P.ParsecT String u Data.Functor.Identity.Identity [[Pos]]
parser = P.many1 $ P.sepBy1 (liftM2 (,) (numParser <* P.char ',') numParser) (P.string " -> ") <* P.endOfLine

parse :: String -> [[Pos]]
parse = either (error . show) id . P.parse parser "input14.txt"

pairs :: [a] -> [(a, a)]
pairs (x : y : z) = (x, y) : pairs (y : z)
pairs _ = []

stroke :: (Pos, Pos) -> [Pos]
stroke ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) [min y1 y2 .. max y1 y2]
  | y1 == y2 = map (,y1) [min x1 x2 .. max x1 x2]
  | otherwise = error "not a straight line"

fallsTo :: S.Set Pos -> Pos -> Maybe Pos
fallsTo blocked from = find (`S.notMember` blocked) $ map (from @+) [(0, 1), (-1, 1), (1, 1)]

fall1 :: Int -> S.Set Pos -> Pos -> Maybe Pos
fall1 stopAt blocked from = case fallsTo blocked from of
  Just to | snd to > stopAt -> Nothing -- falls into abyss
  Just to -> fall1 stopAt (S.insert from blocked) to -- falls
  Nothing -> Just from -- blocked

fall2 :: Int -> S.Set Pos -> Pos -> Maybe Pos
fall2 floor blocked from = case fallsTo blocked from of
  Just to | snd to == floor -> Just from -- blocked by floor
  Just to -> fall2 floor (S.insert from blocked) to -- falls
  Nothing | snd from == 0 -> Nothing -- didn't move at all
  Nothing -> Just from -- blocked

fill :: (S.Set Pos -> Pos -> Maybe Pos) -> S.Set Pos -> Int
fill fall blocked = maybe 0 (\new -> fill fall (S.insert new blocked) + 1) (fall blocked (500, 0))

part1 :: String -> Int
part1 s = fill (fall1 (maximum (S.map snd rocks))) rocks where rocks = S.fromList $ concatMap stroke $ concatMap pairs $ parse s

part2 :: String -> Int
part2 s = 1 + fill (fall2 (2 + maximum (S.map snd rocks))) rocks where rocks = S.fromList $ concatMap stroke $ concatMap pairs $ parse s

main :: IO ()
main = do
  contents <- readFile "src/input14.txt"
  print $ part1 contents
  print $ part2 contents
