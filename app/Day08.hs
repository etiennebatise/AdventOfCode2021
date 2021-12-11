module Day08 where

import Data.Bool
import Data.List
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Util

newtype Outputs = Outputs [String] deriving (Show)

newtype Signals = Signals [String] deriving (Show)

data Row = Row Signals Outputs deriving (Show)

newtype Puzzle = Puzzle [Row] deriving (Show)

signals :: Parser Signals
signals = Signals <$> many letter `sepBy` char ' '

outputs :: Parser Outputs
outputs = Outputs <$> many letter `sepBy` char ' '

puzzle :: Parser Puzzle
puzzle = Puzzle <$> many (Row <$> signals <* string "| " <*> outputs <* try newline)

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/08.txt"

-- Part One
numberOfSegments :: Int -> Int
numberOfSegments 1 = 2
numberOfSegments 4 = 4
numberOfSegments 7 = 3
numberOfSegments 8 = 7
numberOfSegments i = error $ show i ++ " number of segments not handled"

uniqueSegments = [1, 4, 7, 8]

uniqueNumberOfSegments :: String -> Bool
uniqueNumberOfSegments s = length s `elem` (numberOfSegments <$> uniqueSegments)

partOne :: Puzzle -> Int
partOne (Puzzle rows) = sum $ map (\(Row _ (Outputs os)) -> length $ filter uniqueNumberOfSegments os) rows

-- Part Two
length' i = length >.> (== i)

and' f g a = f a && g a

match' s = (`all` s) . flip elem

partTwoRow :: Row -> Int
partTwoRow (Row (Signals signals) (Outputs outputs)) = read $ map go outputs
  where
    Just zero = find (length' 6 `and'` match' one) $ signals \\ [nine]
    Just one = find (length' 2) signals
    Just two = find (length' 5 `and'` elem eSegmentId) signals
    Just three = find (length' 5 `and'` match' seven) signals
    Just four = find (length' 4) signals
    five = head $ filter (length' 5) $ signals \\ [two, three]
    six = head $ filter (length' 6) $ signals \\ [zero, nine]
    Just seven = find (length' 3) signals
    Just eight = find (length' 7) signals
    Just nine = find (length' 6 `and'` match' four) signals
    eSegmentId = head $ eight \\ nine
    go s
      | sort s == sort zero = '0'
      | sort s == sort one = '1'
      | sort s == sort two = '2'
      | sort s == sort three = '3'
      | sort s == sort four = '4'
      | sort s == sort five = '5'
      | sort s == sort six = '6'
      | sort s == sort seven = '7'
      | sort s == sort eight = '8'
      | sort s == sort nine = '9'
      | otherwise =
        let f = intercalate ", " [sort zero, sort one, sort two, sort three, sort four, sort five, sort six, sort seven, sort eight, sort nine]
            b = s == five
         in error $ show $ "s: " ++ sort s ++ " | " ++ show b ++ " | " ++ f

partTwo :: Puzzle -> Int
partTwo (Puzzle row) = sum $partTwoRow <$> row

main :: IO ()
main = do
  putStrLn "Day Five"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ partOne puzzle
  putStr "Part two: "
  print $ partTwo puzzle
