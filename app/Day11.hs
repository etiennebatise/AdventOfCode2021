{-# LANGUAGE TupleSections #-}

module Day11 where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad (join, replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State.Lazy
  ( MonadState (state),
    State,
    evalState,
    runState,
  )
import Data.Array
  ( Array,
    accum,
    assocs,
    bounds,
    elems,
    listArray,
  )
import Data.Bifunctor (Bifunctor (bimap))
import Data.Function ((&))
import Data.List ()
import Debug.Trace (trace)
import Text.Parsec (digit, many, newline, parse, sepBy, try)
import Text.Parsec.Combinator ()
import Text.Parsec.String (Parser)
import Util (unsafeRight, (>.>))

newtype Puzzle = Puzzle (Array (Int, Int) Int)

instance Show Puzzle where
  show (Puzzle arr) = mconcat . unlines' $ show <$> elems arr
    where
      n = (1 +) $ uncurry (-) $ snd $ bounds arr
      y = ['\n']
      unlines' :: [String] -> [String]
      unlines' xs = countdown n xs
        where
          countdown 0 xs = y : countdown n xs -- reset to original n
          countdown _ [] = []
          countdown m (x : xs) = x : countdown (m -1) xs

int :: Parser Int
int = read . (: []) <$> digit

puzzle :: Parser Puzzle
puzzle = go . filter (/= []) <$> many int `sepBy` try newline
  where
    go :: [[Int]] -> Puzzle
    go l = Puzzle $ listArray ((0, 0), (length (head l) - 1, length l - 1)) (join l)

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/11.txt"

-- Lib

adjacentCoords :: (Int, Int) -> [(Int, Int)]
adjacentCoords position = applyToPosition <$> replicateM 2 [flip (-) 1, (+ 0), (+ 1)]
  where
    applyToPosition = flip (uncurry bimap) position . toTuple
    toTuple [a, b] = (a, b)

runStep :: State Puzzle Int
runStep = state (\(Puzzle p) -> let p' = step p in (flashes p', Puzzle p'))
  where
    increaseEnergy = (+ 1)

    modifiers :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [((Int, Int), Int)]
    modifiers ((li, lj), (ui, uj)) coord = (,1) <$> filter positionInGrid (adjacentCoords coord)
      where
        positionInGrid (i, j) = li <= i && i <= ui && lj <= j && j <= uj

    flashing = (== 10)
    maxTen i = if i >= 10 then 10 else i

    transmitEnergy' m p =
      let p' = accum (\e a -> maxTen $ e + a) p m
          m' = modifiers (bounds p) =<< map fst (filter (snd >.> flashing) (assocs p'))
       in if m' == m
            then p'
            else transmitEnergy' m' p

    resetEnergy p = (\v -> if v > 9 then 0 else v) <$> p

    step puzzle = resetEnergy $ transmitEnergy' [] $ increaseEnergy <$> puzzle

    hasFlashed = (== 0)

    flashes puzzle = length $ filter hasFlashed $ elems puzzle

-- Part One

partOne :: Puzzle -> Int
partOne = sum . evalState (replicateM 100 runStep)

-- Part Two

partTwo :: Puzzle -> Int
partTwo puzzle@(Puzzle arr) =
  let size = length $ elems arr
   in (1 +) $ length $ evalState (unfoldWhileM (/= size) runStep) puzzle

main :: IO ()
main = do
  putStrLn "Day Eleven"
  puzzle@(Puzzle arr) <- getPuzzle
  print puzzle
  putStr "Part one: "
  print $ partOne puzzle
  putStr "Part two: "
  print $ partTwo puzzle
