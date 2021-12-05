module Day01 (main) where

type Puzzle = [Int]

getPuzzle :: IO Puzzle
getPuzzle = map read . lines <$> readFile "./assets/01.txt"

solve :: Int -> Puzzle -> Int
solve w p = length $ filter (< 0) $ zipWith (-) p (drop w p)

main :: IO ()
main = do
  putStrLn "Day One"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ solve 1 puzzle
  putStr "Part two: "
  print $ solve 3 puzzle
