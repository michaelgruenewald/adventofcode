{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Day1 qualified

main :: IO ()
main = do
  contents <- readFile "input1.txt"
  print $ Day1.part1 contents
  print $ Day1.part2 contents
