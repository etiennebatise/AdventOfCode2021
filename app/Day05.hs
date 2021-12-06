module Day05 where

import Data.Bool
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
import Text.Parsec
import Text.Parsec.String
import Util

data Position = Position Int Int deriving (Show, Eq, Ord)

data Vector = Vector Position Position deriving (Show)

newtype Puzzle = Puzzle [Vector] deriving (Show)

-- Parser
position :: Parser Position
position = Position <$> number <* char ',' <*> number

vector :: Parser Vector
vector = Vector <$> position <* string " -> " <*> position <* try newline

puzzle :: Parser Puzzle
puzzle = Puzzle <$> many1 vector

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/05.txt"

-- Lib
-- range' (1, 3) = [1, 2, 3]
-- range' (3, 1) = [3, 2, 1]
range' :: (Int, Int) -> [Int]
range' x = bool (reverse . range . swap) range (uncurry (<) x) x

horizontalAndVertical :: Vector -> [Position]
horizontalAndVertical (Vector p1@(Position x1 y1) p2@(Position x2 y2)) =
  case (x1 == x2, y1 == y2) of
    (False, False) -> []
    (True, False) -> Position x1 <$> range' (y1, y2)
    (False, True) -> flip Position y1 <$> range' (x1, x2)
    (True, True) -> [p1, p2]

horizontalAndVerticalAndDiagonal :: Vector -> [Position]
horizontalAndVerticalAndDiagonal (Vector p1@(Position x1 y1) p2@(Position x2 y2)) =
  case (x1 == x2, y1 == y2) of
    (False, False) -> zipWith Position (range' (x1, x2)) (range' (y1, y2))
    (True, False) -> Position x1 <$> range' (y1, y2)
    (False, True) -> flip Position y1 <$> range' (x1, x2)
    (True, True) -> [p1, p2]

-- Solve
solveParts :: Puzzle -> (Vector -> [Position]) -> Int
solveParts (Puzzle vs) getPoints = length $ filter ((> 1) . length) $ group $ sort $ getPoints =<< vs

main :: IO ()
main = do
  putStrLn "Day Five"
  puzzle <- getPuzzle
  let (Puzzle vectors) = puzzle
  putStr "Part one: "
  print $ solveParts puzzle horizontalAndVertical
  putStr "Part two: "
  print $ solveParts puzzle horizontalAndVerticalAndDiagonal
