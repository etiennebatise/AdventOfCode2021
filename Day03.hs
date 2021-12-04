module Day03 where

import Data.Bool
import Data.Char
import Data.List
import Data.Monoid
import Numeric
import Util

getPuzzle :: IO [String]
getPuzzle = lines <$> readFile "./03.txt"

-- Part One

parseLine :: [Sum Int] -> String -> [Sum Int]
parseLine a = zipWith (<>) a . map (Sum . read . (:""))

sumLines :: [String] -> [Int]
sumLines = map getSum . foldr (flip parseLine) (repeat mempty)

computeGammaRate :: [String] -> [Int]
computeGammaRate ls = (bool 0 1 . (length ls `div` 2 <)) <$> sumLines ls

solvePartOne :: [String] -> Int
solvePartOne puzzle = binToDec gammaRate * binToDec epsilonRate
  where
    gammaRate = computeGammaRate puzzle
    epsilonRate = map (1 -) gammaRate

-- Part two
type Row = [Int]

parts :: Int -> Row -> ([Row], [Row]) -> ([Row], [Row])
parts index row (zeros, ones) =
  bool (row:zeros, ones) (zeros, row:ones) (row !! index == 1) 

type RatingRule = [Row] -> [Row] -> [Row]

oxygen :: RatingRule
oxygen zeros ones = bool zeros ones (length ones >= length zeros)

carbon :: RatingRule
carbon zeros ones = bool ones zeros (length zeros <= length ones)

computeRating :: RatingRule -> Int -> [Row] -> [Int]
computeRating rule _ (row:[]) = row
computeRating rule i rows = computeRating rule (i+1) next
  where 
    next = uncurry rule $ foldr (parts i) ([], []) rows

solvePartTwo :: [String] -> Int
solvePartTwo puzzle = oxygenRating * carbonRating
  where
    rows :: [Row]
    rows = (map . map) (read . (:"")) puzzle
    oxygenRating = binToDec $ computeRating oxygen 0 rows
    carbonRating = binToDec $ computeRating carbon 0 rows

main :: IO ()
main = do
  putStrLn "Day Three"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ solvePartOne puzzle
  putStr "Part two: "
  print $ solvePartTwo puzzle


