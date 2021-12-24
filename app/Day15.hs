{-# OPTIONS_GHC -Wno-typed-holes #-}

-- |
module Day15 where

import Control.Monad (join, replicateM)
import Data.Array (Array, array, assocs, bounds, indices, listArray)
import Data.Bifunctor (bimap, first)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map (Map, adjust, delete, empty, insert, insertWith, member, notMember, size, toList, (!), (!?))
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.Traversable (sequenceA)
import Data.Tuple.Extra (dupe, second)
import Day04 (puzzleP)
import Debug.Trace
import Text.Parsec (digit, many1, newline, parse, try)
import Text.Parsec.String (Parser)
import Util (unsafeRight)

type Point = (Int, Int)

type Puzzle = Array Point Int

number :: Parser Int
number = read . (: []) <$> digit

puzzle :: Parser Puzzle
puzzle = go <$> many1 (many1 number <* try newline)
  where
    go :: [[Int]] -> Puzzle
    go l = listArray ((1, 1), (length $ head l, length l)) (join l)

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/15.txt"

-- Lib

inBounds :: Point -> Point -> Point -> Bool
inBounds (li, lj) (ui, uj) (i, j) = li <= i && i <= ui && lj <= j && j <= uj

inBounds' = uncurry inBounds

adjacentCoords :: (Int, Int) -> [(Int, Int)]
adjacentCoords point = applyToPoint point <$> [goLeft, goRight, goUp, goDown]
  where
    goUp = (flip (-) 1, (+ 0))
    goDown = ((+ 1), (+ 0))
    goLeft = ((+ 0), flip (-) 1)
    goRight = ((+ 0), (+ 1))

    applyToPoint point = flip (uncurry bimap) point

type Graph = Map Point (Maybe Int)

dijkstra :: Point -> Point -> Puzzle -> Int
dijkstra start end puzzle = go initialGrah
  where
    puzzleGraph = foldr (uncurry insert) empty $ assocs puzzle
    initialGrah = insert start (Just 0) $ foldr (`insert` Nothing) empty $ indices puzzle

    go :: Graph -> Int
    go graph =
      let (point, score) = lowestRisk graph
          graphWithoutCurrentLowest = delete point graph
          points = neighbours point score
          newGraph = updateGraph graphWithoutCurrentLowest points
          done = end == fst (lowestRisk newGraph) || size newGraph == 1
       in if done then snd $ lowestRisk newGraph else go newGraph
    -- in if size newGraph == 1 then extractRemainingPoint newGraph else go newGraph

    extractRemainingPoint = fromJust . snd . head . toList

    lowestRisk :: Graph -> (Point, Int)
    lowestRisk = minimumBy (compare `on` snd) . mapMaybe sequence . toList

    neighbours point score =
      let points = filter (inBounds' (bounds puzzle)) (adjacentCoords point)
       in (\p -> (p, score + puzzleGraph ! p)) <$> points

    updateGraph graph points =
      let scoreRule newVal oldVal = if newVal < oldVal then newVal else oldVal
          updateScore newVal = maybe (Just newVal) (Just . scoreRule newVal)
          upsertIfLessRisky' (point, newScore) = adjust (updateScore newScore) point
       in foldr upsertIfLessRisky' graph points

partOne :: Puzzle -> Int
partOne puzzle = dijkstra start end puzzle
  where
    (start, end) = bounds puzzle

increment n = head . take (n + 1) $ cycle [1 .. 9]

translate :: (Int, Int) -> Point -> Point
translate (vi, vj) (i, j) = (vi + i, vj + j)

grow :: Int -> Puzzle -> Puzzle
grow n puzzle = array (lowerBound, ((n + 1) * height, (n + 1) * width)) newPuzzle'
  where
    puzzle' = assocs puzzle

    (lowerBound, upperBound) = bounds puzzle
    height = fst upperBound - fst lowerBound + 1
    width = snd upperBound - snd lowerBound + 1

    transaletPuzzleHorizontally :: Int -> [(Point, Int)] -> [(Point, Int)]
    transaletPuzzleHorizontally n = fmap (first (translate (0, n * width)))

    transaletPuzzleHorizontallyTimes :: Int -> [(Point, Int)] -> [(Point, Int)]
    transaletPuzzleHorizontallyTimes n p = flip transaletPuzzleHorizontally p =<< [0 .. n]

    transaletPuzzleVertically :: Int -> [(Point, Int)] -> [(Point, Int)]
    transaletPuzzleVertically n = fmap (first (translate (n * height, 0)))

    transaletPuzzleVerticallyTimes :: Int -> [(Point, Int)] -> [(Point, Int)]
    transaletPuzzleVerticallyTimes n p = flip transaletPuzzleVertically p =<< [0 .. n]

    newPuzzle' = transaletPuzzleVerticallyTimes n $ transaletPuzzleHorizontallyTimes n puzzle'

partTwo puzzle = dijkstra start end grownPuzzle
  where
    grownPuzzle = grow 5 puzzle
    (start, end) = bounds grownPuzzle

debug s v = trace (s ++ " " ++ show v) v

main :: IO ()
main = do
  putStrLn "Day Fourteen"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ partOne puzzle
  putStr "Part two: "
  print $ partTwo puzzle
