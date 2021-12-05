{-# LANGUAGE LambdaCase #-}

module Day04 where

import Control.Lens
import Data.Bool
import Data.List
import Data.Monoid
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Util

data Markable = Unmarked Int | Marked deriving (Show, Eq)

instance Semigroup Markable where
  (<>) m Marked = m
  (<>) Marked m = m
  (<>) (Unmarked i) (Unmarked j) = Unmarked $ i + j

instance Monoid Markable where
  mempty = Unmarked 0

isMarked :: Markable -> Bool
isMarked Marked = True
isMarked _ = False

type Row = [Markable]

type Board = [Row]

data Puzzle = Puzzle
  { draws :: [Int],
    boards :: [Board]
  }
  deriving (Show)

-- Load Puzzle
markable :: Parser Markable
markable = Unmarked <$> number

drawsP :: Parser [Int]
drawsP = number `sepBy1` char ','

rowP :: Parser Row
rowP = count 5 $ spaces *> markable

boardP :: Parser Board
boardP = count 5 rowP

puzzleP :: Parser Puzzle
puzzleP = Puzzle <$> (drawsP <* newline) <*> manyTill boardP (try (skipMany newline >> eof))

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzleP "" <$> readFile "./assets/04.txt"

-- Lib
markRow :: Int -> Row -> Row
markRow i = fmap (\a -> bool a Marked (a == Unmarked i))

markBoard :: Int -> Board -> Board
markBoard = fmap . markRow

markBoards :: Int -> [Board] -> [Board]
markBoards = fmap . markBoard

rowIsComplete :: Row -> Bool
rowIsComplete = all isMarked

boardIsWinner :: Board -> Bool
boardIsWinner board = any rowIsComplete board || any rowIsComplete (transpose board)

boardProduct :: Board -> Int
boardProduct board = let Unmarked product = mconcat $ fmap mconcat board in product

result :: Int -> Board -> Int
result currentDraw board = currentDraw * boardProduct board

solveParts :: (Monoid (m Int)) => (Int -> m Int) -> Puzzle -> m Int
solveParts monoid (Puzzle draws boards) = snd $ foldl go (boards, mempty) draws
  where
    go (boards, m) called =
      let marked = markBoards called boards
          winners = filter boardIsWinner marked
          notWinningBoards = marked \\ winners
          winnerScore = mconcat $ map (monoid . result called) winners
       in (notWinningBoards, m <> winnerScore)

main :: IO ()
main = do
  putStrLn "Day Four"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ solveParts (First . Just) puzzle
  putStr "Part two: "
  print $ solveParts (Last . Just) puzzle
