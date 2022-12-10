module Day10 (main) where

import Data.List.Extra (chunksOf, intercalate)

parse :: String -> [[String]]
parse = map words . lines

states :: Int -> [[String]] -> [Int]
states i [] = []
states i (["noop"] : xs) = i : states i xs
states i (["addx", v] : xs) = i : i : states (i + read v) xs
states _ _ = error "invalid instruction"

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . filter (\(cycle, _) -> cycle `mod` 40 == 20) . zip [1 ..] . states 1 . parse

part2 :: String -> String
part2 = intercalate "\n" . chunksOf 40 . zipWith (\cycle position -> if abs (cycle `mod` 40 - position) <= 1 then '█' else '░') [0 ..] . states 1 . parse

main :: IO ()
main = do
  contents <- readFile "src/input10.txt"
  print $ part1 contents
  putStrLn $ part2 contents
