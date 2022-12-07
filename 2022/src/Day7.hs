module Day7 (main) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.List.Extra (dropEnd1)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data State = State {cwd :: [String], dirs :: M.Map [String] (M.Map String Int)} deriving (Show)

parse :: String -> State
parse s = foldl step initial $ lines s
  where
    initial = State [] M.empty
    step state line = case words line of
      ["$", "cd", ".."] -> state {cwd = dropEnd1 (cwd state)}
      ["$", "cd", "/"] -> ensureDir $ state {cwd = []}
      ["$", "cd", name] -> ensureDir $ state {cwd = cwd state ++ [name]}
      ["$", "ls"] -> state
      "$" : _ -> error ("Unknown command: " ++ line)
      ["dir", name] -> state
      [size, name] | all isDigit size -> state {dirs = M.adjust (M.insert name (read size)) (cwd state) (dirs state)}
      _ -> error ("Unknown line: " ++ line)
    ensureDir state = state {dirs = M.alter (Just . fromMaybe M.empty) (cwd state) (dirs state)}

dirSize :: State -> [String] -> Int
dirSize state k = M.foldrWithKey (\dir files accum -> accum + if k `isPrefixOf` dir then sum $ M.elems files else 0) 0 (dirs state)

part1 :: String -> Int
part1 s = sum . filter (<= 100000) $ map (dirSize state) (M.keys $ dirs state) where state = parse s

part2 :: String -> Int
part2 s = minimum . filter (>= dirSize state [] + 30000000 - 70000000) $ map (dirSize state) (M.keys $ dirs state) where state = parse s

main :: IO ()
main = do
  contents <- readFile "src/input7.txt"
  print $ part1 contents
  print $ part2 contents
