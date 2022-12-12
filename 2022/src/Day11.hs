{-# LANGUAGE TemplateHaskell #-}

module Day11 (main) where

import Control.Lens (element, makeLenses, over, set, (^.))
import Data.Functor (($>), (<&>))
import qualified Data.Functor.Identity
import Data.List (nub, sort)
import qualified Text.Parsec as P

data Operand = Old | Literal Int deriving (Show)

opValue :: Operand -> Int -> Int
opValue Old x = x
opValue (Literal l) _ = l

type Operation = (Operand, Char, Operand)

applyTo :: Operation -> Int -> Int
applyTo (a, '+', b) old = opValue a old + opValue b old
applyTo (a, '*', b) old = opValue a old * opValue b old
applyTo _ _ = error "invalid operation"

data MonkeyBehavior = MonkeyBehavior {_monkeyId :: Int, _initialItems :: [Int], _operation :: Operation, _test :: Int, _trueMonkey :: Int, _falseMonkey :: Int} deriving (Show)

makeLenses ''MonkeyBehavior

data MonkeyState = MonkeyState {_passed :: Int, _items :: [Int]} deriving (Show)

makeLenses ''MonkeyState

parser :: P.ParsecT String u Data.Functor.Identity.Identity [MonkeyBehavior]
parser =
  P.many
    ( do
        monkey <- P.string "Monkey" <* P.spaces >> P.many1 P.digit <* P.char ':' <* P.spaces <&> read
        starts <- P.string "Starting items: " >> P.sepBy1 (P.many1 P.digit <&> read) (P.string "," >> P.spaces) <* P.spaces
        op1 <- P.string "Operation: new = " >> P.choice [P.string "old" $> Old, P.many1 P.digit <&> Literal . read] <* P.spaces
        op <- P.oneOf "+-*" <* P.spaces
        op2 <- P.choice [P.string "old" $> Old, P.many1 P.digit <&> Literal . read] <* P.spaces
        div <- P.string "Test: divisible by" <* P.spaces >> P.many1 P.digit <* P.spaces <&> read
        trueMonkey <- P.string "If true: throw to monkey" >> P.spaces >> P.many1 P.digit <* P.spaces <&> read
        falseMonkey <- P.string "If false: throw to monkey" >> P.spaces >> P.many1 P.digit <* P.spaces <&> read
        return (MonkeyBehavior monkey starts (op1, op, op2) div trueMonkey falseMonkey)
    )
    <* P.eof

parse :: String -> [MonkeyBehavior]
parse = either (error . show) id . P.parse parser "input11.txt"

run :: (Int -> Int) -> Int -> String -> Int
run adjustment rounds s =
  let behaviors = parse s
      initial = map (MonkeyState 0 . (^. initialItems)) behaviors
      ring = product $ nub $ map (^. test) behaviors
      round states = foldl turn states behaviors
      turn states behavior = foldl (lookAt behavior) states (states !! (behavior ^. monkeyId) ^. items)
      lookAt behavior states item =
        let newItem = adjustment $ applyTo (behavior ^. operation) item `mod` ring
            nextMonkey = behavior ^. if newItem `mod` behavior ^. test == 0 then trueMonkey else falseMonkey
            countItem = over passed (+ 1)
            dropItem = over items tail
            throwItem = over items (++ [newItem])
         in over (element (behavior ^. monkeyId)) (countItem . dropItem) . over (element nextMonkey) throwItem $ states
   in product $ take 2 $ reverse $ sort $ map (^. passed) $ iterate round initial !! rounds

part1 :: String -> Int
part1 = run (`div` 3) 20

part2 :: String -> Int
part2 = run id 10000

main :: IO ()
main = do
  contents <- readFile "src/input11.txt"
  print $ part1 contents
  print $ part2 contents
