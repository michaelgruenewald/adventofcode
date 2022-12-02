{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Day1 qualified
import Day2 qualified

main :: IO ()
main = do
  day1
  day2

day1 :: IO ()
day1 = do
  contents <- readFile "input1.txt"
  print $ Day1.part1 contents
  print $ Day1.part2 contents

day2 :: IO ()
day2 = do
  contents <- readFile "input2.txt"
  print $ Day2.part1 contents
  print $ Day2.part2 contents
