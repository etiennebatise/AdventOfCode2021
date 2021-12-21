{-# LANGUAGE TupleSections #-}

module Day09 where

import Control.Monad
import Data.Array
import Data.Bool
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Language (LanguageDef)
import Text.Parsec.String
import Util (unsafeRight)

data Puzzle = Puzzle Int Int [Int] deriving (Show) -- Nb lines, length lines, digits

-- Load
puzzle :: Parser Puzzle
puzzle = go <$> many1 (many1 (read . (: []) <$> digit) <* try newline)
  where
    go rows = Puzzle (length rows) (length $ head rows) (join rows)

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/09.txt"

-- Lib
coord :: Puzzle -> Int -> (Int, Int)
coord (Puzzle height width _) i =
  let h = (i `div` width)
      w = i - (h * width)
   in (h, w)

lookup' :: Puzzle -> (Int, Int) -> Maybe Int
lookup' (Puzzle height width ints) (h, w)
  | h < 0 || h > height - 1 || w < 0 || w > width - 1 = Nothing
  | otherwise = Just $ ints !! (h * width + w)

adjacentCoords :: (Int, Int) -> [(Int, Int)]
adjacentCoords (h, w) = [(h - 1, w), (h + 1, w), (h, w + 1), (h, w -1)]

fetchAdjacentPoints :: Puzzle -> (Int, Int) -> [(Int, (Int, Int))]
fetchAdjacentPoints p = mapMaybe (\c -> (,c) <$> lookup' p c) . adjacentCoords

lowPoints :: Puzzle -> [(Int, (Int, Int))]
lowPoints p@(Puzzle height width ints) = mapMaybe go $ zip ints [0 ..]
  where
    -- go :: (Int, Int) -> Maybe (Int, (Int, Int))
    go (v, i) =
      let (h, w) = coord p i
          points = fetchAdjacentPoints p (h, w)
          isLowPoint = all ((v <) . fst) points
       in bool Nothing (Just (v, (h, w))) isLowPoint

-- Part One

partOne :: Puzzle -> Int
partOne = sum . map ((+ 1) . fst) . lowPoints

-- Part Two
basin :: Puzzle -> (Int, (Int, Int)) -> [(Int, (Int, Int))]
basin puzzle (v, coord) =
  let points = fetchAdjacentPoints puzzle coord
      higherPoints = filter ((\p -> v < p && p /= 9) . fst) points
      next = nub $ basin puzzle =<< higherPoints
   in (v, coord) : next

partTwo :: Puzzle -> Int
partTwo p = product $ take 3 $ reverse $ sort $ map (length . basin p) $ lowPoints p

-- Main
main :: IO ()
main = do
  putStrLn "Day Nine"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ partOne puzzle
  putStr "Part two: "
  print $ partTwo puzzle
