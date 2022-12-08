module Day7 (main) where

import Data.Char (isDigit)
import Data.List (foldl', isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data ParseState = ParseState {cwd :: [String], dirSizes :: M.Map [String] Int}

parse :: String -> M.Map [String] Int
parse = dirSizes . foldl' step initial . lines
  where
    initial = ParseState [] M.empty
    step state line = case words line of
      ["$", "cd", ".."] -> state {cwd = init (cwd state)}
      ["$", "cd", "/"] -> ensureDir $ state {cwd = []}
      ["$", "cd", name] -> ensureDir $ state {cwd = cwd state ++ [name]}
      ["$", "ls"] -> state
      "$" : _ -> error ("Unknown command: " ++ line)
      ["dir", name] -> state
      [size, name] | all isDigit size -> state {dirSizes = M.adjust (+ read size) (cwd state) (dirSizes state)}
      _ -> error ("Unknown line: " ++ line)
    ensureDir state = state {dirSizes = M.alter (Just . fromMaybe 0) (cwd state) (dirSizes state)}

treeSize :: M.Map [String] Int -> [String] -> Int
treeSize sizes root = sum $ M.filterWithKey (\dir _ -> root `isPrefixOf` dir) sizes

part1 :: String -> Int
part1 s = sum . filter (<= 100000) $ map (treeSize dirs) (M.keys dirs) where dirs = parse s

part2 :: String -> Int
part2 s = minimum . filter (>= treeSize dirs [] + 30000000 - 70000000) $ map (treeSize dirs) (M.keys dirs) where dirs = parse s

main :: IO ()
main = do
  contents <- readFile "src/input7.txt"
  print $ part1 contents
  print $ part2 contents
