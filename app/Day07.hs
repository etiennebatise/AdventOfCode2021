-- |
module Day07 where

import Data.Bool
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Util

newtype Puzzle = Puzzle [Int]

puzzle :: Parser Puzzle
puzzle = Puzzle <$> number `sepBy` char ','

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/07.txt"

mean :: [Int] -> Int
mean l = sum l `div` length l

-- Part One
median :: [Int] -> Int
median l' = bool caseOdd caseEven (even $ length l)
  where
    l = sort l'
    caseEven = mean [caseOdd, l !! (length l `div` 2)]
    caseOdd = l !! ((length l `div` 2) - 1)

-- Part Two
fermat :: Int -> Int
fermat n = n + (n - 1) * n `div` 2

partTwo :: Puzzle -> Int
partTwo (Puzzle ints) = snd $ fromJust $ foldr go Nothing [(foldr min 0 ints) .. (foldr max 0 ints)]
  where
    totalCost crab = sum $ map (fermat . abs . (crab -)) ints

    go :: Int -> Maybe (Int, Int) -> Maybe (Int, Int)
    go currentCrab Nothing = Just (currentCrab, totalCost currentCrab)
    go currentCrab (Just best@(bestCrab, bestCost)) =
      let currentTotalCost = totalCost currentCrab
       in Just $ bool best (currentCrab, currentTotalCost) (currentTotalCost < bestCost)

main :: IO ()
main = do
  putStrLn "Day Seven"
  puzzle <- getPuzzle
  let Puzzle ints = puzzle
  let m = median ints
  putStr "Part one: "
  print $ sum $ fmap (abs . (m -)) ints
  putStr "Part two: "
  print $ partTwo puzzle
