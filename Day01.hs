module Day01 (dayOne) where 

type Puzzle = [Int]

getPuzzle :: IO Puzzle
getPuzzle = (map read) <$> lines <$> readFile "./01.txt"

solve :: Int -> Puzzle -> Int
solve w p = length $ filter (<0) $ zipWith (\a b -> a - b) p (drop w p)

dayOne :: IO ()
dayOne = do
  putStrLn "Day One"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ solve 1 puzzle
  putStr "Part two: "
  print $ solve 3 puzzle
