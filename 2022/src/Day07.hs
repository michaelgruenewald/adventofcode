{-# LANGUAGE TemplateHaskell #-}

module Day07 (main) where

import Control.Lens
import Data.Char (isDigit)
import Data.List (foldl', isPrefixOf)
import Data.Map (Map, adjust, alter, empty, filterWithKey, keys)
import Data.Maybe (fromMaybe)

data ParseState = ParseState {_cwd :: [String], _dirSizes :: Map [String] Int}

makeLenses ''ParseState

parse :: String -> Map [String] Int
parse = (^. dirSizes) . foldl' step initial . lines
  where
    initial = ParseState [] empty
    step state line =
      ensureDir
        $ case words line of
          ["$", "cd", "/"] -> set cwd []
          ["$", "cd", ".."] -> over cwd init
          ["$", "cd", name] -> over cwd (++ [name])
          ["$", "ls"] -> id
          "$" : _ -> error ("Unknown command: " ++ line)
          ["dir", name] -> id
          [size, name] | all isDigit size -> over dirSizes (adjust (+ read size) (state ^. cwd))
          _ -> error ("Unknown line: " ++ line)
        $ state
    ensureDir state = over dirSizes (alter (Just . fromMaybe 0) (state ^. cwd)) state

treeSize :: Map [String] Int -> [String] -> Int
treeSize sizes root = sum $ filterWithKey (\dir _ -> root `isPrefixOf` dir) sizes

part1 :: String -> Int
part1 s = sum . filter (<= 100000) $ map (treeSize dirs) (keys dirs) where dirs = parse s

part2 :: String -> Int
part2 s = minimum . filter (>= treeSize dirs [] + 30000000 - 70000000) $ map (treeSize dirs) (keys dirs) where dirs = parse s

main :: IO ()
main = do
  contents <- readFile "src/input07.txt"
  print $ part1 contents
  print $ part2 contents
