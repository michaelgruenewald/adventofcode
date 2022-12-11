{-# LANGUAGE TemplateHaskell #-}

module Day11 (main) where

import Control.Lens (element, makeLenses, over, set, (^.))
import Data.Functor (($>), (<&>))
import qualified Data.Functor.Identity
import Data.List (nub, sort)
import qualified Text.Parsec as Parsec

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

data MonkeyState = MonkeyState {_items :: [Int], _passed :: Int} deriving (Show)

makeLenses ''MonkeyState

parser :: Parsec.ParsecT String u Data.Functor.Identity.Identity [MonkeyBehavior]
parser = do
  monkeys <-
    Parsec.many
      ( do
          monkey <- Parsec.string "Monkey " >> Parsec.many1 Parsec.digit <&> read
          Parsec.char ':'
          Parsec.spaces
          starts <- Parsec.string "Starting items: " >> Parsec.sepBy1 (Parsec.many1 Parsec.digit <&> read) (Parsec.string "," >> Parsec.spaces)
          Parsec.spaces
          op1 <- Parsec.string "Operation: new = " >> Parsec.choice [Parsec.string "old" $> Old, Parsec.many1 Parsec.digit <&> Literal . read]
          Parsec.spaces
          op <- Parsec.oneOf "+-*"
          Parsec.spaces
          op2 <- Parsec.choice [Parsec.string "old" $> Old, Parsec.many1 Parsec.digit <&> Literal . read]
          Parsec.spaces
          Parsec.string "Test: divisible by"
          Parsec.spaces
          div <- Parsec.many1 Parsec.digit <&> read
          Parsec.spaces
          Parsec.string "If true: throw to monkey"
          Parsec.spaces
          trueMonkey <- Parsec.many1 Parsec.digit <&> read
          Parsec.spaces
          Parsec.string "If false: throw to monkey"
          Parsec.spaces
          falseMonkey <- Parsec.many1 Parsec.digit <&> read
          Parsec.spaces
          return (MonkeyBehavior monkey starts (op1, op, op2) div trueMonkey falseMonkey)
      )
  Parsec.eof
  return monkeys

parse :: String -> [MonkeyBehavior]
parse s = case Parsec.parse parser "input11.txt" s of
  Left x -> error (show x)
  Right x -> x

run :: (Int -> Int) -> Int -> String -> Int
run adjustment rounds s =
  let behaviors = parse s
      initial = map (\b -> MonkeyState (b ^. initialItems) 0) behaviors
      ring = product $ nub $ map (^. test) behaviors
      round ms = foldl turn ms [0 .. length behaviors - 1]
      turn ms mid = foldl (lookAt mid) ms (ms !! mid ^. items)
      lookAt mid ms item =
        let behavior = (^.) (behaviors !! mid)
            newItem = adjustment $ applyTo (behavior operation) item `mod` ring
            nextMonkey = if newItem `mod` behavior test == 0 then behavior trueMonkey else behavior falseMonkey
         in over (element mid) (over items tail . over passed (+ 1)) $ over (element nextMonkey) (over items (++ [newItem])) ms
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