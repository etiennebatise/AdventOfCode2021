{-# LANGUAGE DerivingStrategies #-}

-- |
module Day13 where

import Control.Monad (Functor)
import Control.Monad.Zip (MonadZip (mzip))
import Data.Bifunctor (Bifunctor, first, second)
import Data.Bool (bool)
import Data.Functor (($>))
import Data.List (nub)
import Data.Map (Map, assocs, empty, insertWith)
import Data.Tuple.Extra (fst3, snd3)
import Debug.Trace
import Text.Parsec (char, incSourceColumn, many1, newline, parse, sepBy1, string, try, (<|>))
import Text.Parsec.String (Parser)
import Util (number, unsafeRight)

data Axis = X | Y deriving (Show)

type Position = (Int, Int)

data Instruction = Instruction Axis Int deriving (Show)

data Puzzle = Puzzle [Position] [Instruction] deriving (Show)

position :: Parser Position
position = (,) <$> (number <* char ',') <*> number

axis :: Parser Axis
axis = x <|> y
  where
    x = char 'x' $> X
    y = char 'y' $> Y

instruction :: Parser Instruction
instruction = Instruction <$> (string "fold along " *> axis <* char '=') <*> number

puzzle :: Parser Puzzle
puzzle = do
  positions <- many1 (position <* try newline)
  _ <- newline
  instructions <- many1 (instruction <* try newline)
  return $ Puzzle positions instructions

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/13.txt"

solve :: Puzzle -> [Position]
solve (Puzzle dots folds) = foldl (flip go) dots folds
  where
    go :: Instruction -> [Position] -> [Position]
    go (Instruction X line) = fmap (first (sym line))
    go (Instruction Y line) = fmap (second (sym line))

    sym line coord = if coord < line then coord else line - (coord - line)

partOne :: Puzzle -> Int
partOne = length . nub . solve

partTwo :: Puzzle -> String
partTwo puzzle = snd $ foldl go (0, "") grid
  where
    go :: (Int, String) -> (Int, Int, Char) -> (Int, String)
    go (lastLine, s) (_, line, c) = if lastLine /= line then (line, s ++ ['\n', c]) else (line, s ++ [c])

    dots = nub $ solve puzzle

    maxX = maximum $ fst <$> dots
    maxY = maximum $ snd <$> dots

    grid = do
      j <- [0 .. maxY]
      i <- [0 .. maxX]
      let c = if (i, j) `elem` dots then '#' else ' '
      return (i, j, c)

main :: IO ()
main = do
  putStrLn "Day Thirteen"
  puzzle <- getPuzzle
  let (Puzzle dots instructions) = puzzle
  putStr "Part one: "
  print $ partOne (Puzzle dots (take 1 instructions))
  putStrLn "Part two: "
  putStr $ partTwo puzzle
