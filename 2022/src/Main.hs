{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Day1 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified

main :: IO ()
main = do
  day1
  day2
  day3
  day4

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

day3 :: IO ()
day3 = do
  contents <- readFile "input3.txt"
  print $ Day3.part1 contents
  print $ Day3.part2 contents

day4 :: IO ()
day4 = do
  contents <- readFile "input4.txt"
  print $ Day4.part1 contents
  print $ Day4.part2 contents
