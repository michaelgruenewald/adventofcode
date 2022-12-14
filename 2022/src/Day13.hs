module Day13 (main) where

import Control.Monad (liftM2)
import Data.Functor ((<&>))
import qualified Data.Functor.Identity
import Data.List (elemIndices, findIndices, sort)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P

data Packet = N Int | L [Packet] deriving (Eq, Show)

instance Ord Packet where
  compare (N left) (N right) = compare left right
  compare (N left) right = compare (L [N left]) right
  compare left (N right) = compare left (L [N right])
  compare (L []) (L []) = EQ
  compare (L []) (L (_ : _)) = LT
  compare (L (_ : _)) (L []) = GT
  compare (L (left : ls)) (L (right : rs))
    | left == right = compare (L ls) (L rs)
    | otherwise = compare left right

packetParser :: P.ParsecT String u Data.Functor.Identity.Identity Packet
packetParser =
  (P.char '[' >> P.sepBy packetParser (P.char ',') <* P.char ']' <&> L)
    <|> (P.many1 P.digit <&> (N . read))

fileParser :: P.ParsecT String u Data.Functor.Identity.Identity [(Packet, Packet)]
fileParser = P.sepBy1 (liftM2 (,) (packetParser <* P.endOfLine) (packetParser <* P.endOfLine)) P.endOfLine

parse :: String -> [(Packet, Packet)]
parse = either (error . show) id . P.parse fileParser "input13.txt"

part1 :: String -> Int
part1 = sum . map (+ 1) . elemIndices LT . map (uncurry compare) . parse

part2 :: String -> Int
part2 = product . map (+ 1) . findIndices (`elem` dividers) . sort . (dividers ++) . concatMap (\(x, y) -> [x, y]) . parse where dividers = [L [L [N 2]], L [L [N 6]]]

main :: IO ()
main = do
  contents <- readFile "src/input13.txt"
  print $ part1 contents
  print $ part2 contents
