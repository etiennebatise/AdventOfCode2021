module Day10 where

import Data.Bool
import Data.Either (fromLeft)
import Data.Either.Extra
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Util (unsafeRight)

newtype Puzzle = Puzzle [String] deriving (Show)

puzzle :: Parser Puzzle
puzzle = Puzzle <$> many (oneOf "{}[]<>()") `sepBy` try newline

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/10.txt"

-- Lib
opening :: Char -> Bool
opening '<' = True
opening '(' = True
opening '{' = True
opening '[' = True
opening _ = False

closing :: Char -> Char -> Bool
closing '<' '>' = True
closing '(' ')' = True
closing '{' '}' = True
closing '[' ']' = True
closing _ _ = False

newtype Scorer a = Scorer {score :: [Either Char String] -> a}

-- Part One

partOneScore :: Scorer Int
partOneScore = Scorer (sum . map (either points (const 0)))
  where
    points :: Char -> Int
    points ')' = 3
    points ']' = 57
    points '}' = 1197
    points '>' = 25137
    points a = error $ "unknown points for char " ++ show a

-- Part Two

partTwoScore :: Scorer Int
partTwoScore = Scorer (middle . sort . map stackPoints . mapMaybe eitherToMaybe)
  where
    middle l = l !! (length l `div` 2)

    stackPoints :: String -> Int
    stackPoints = foldl (\i c -> 5 * i + points c) 0

    points '(' = 1
    points '[' = 2
    points '{' = 3
    points '<' = 4
    points a = error $ "unknown points for char " ++ show a

-- | Solve
-- >>> solve (Puzzle ["{([(<{}[<>[]}>{[]{[(<()>"]) partOneScore == 1197
-- True
--
-- >>> solve (Puzzle ["[[<[([]))<([[{}[[()]]]"]) partOneScore == 3
-- True
--
-- >>> solve (Puzzle ["[{[{({}]{}}([{[{{{}}([]"]) partOneScore == 57
-- True
--
-- >>> solve (Puzzle ["<{([([[(<>()){}]>(<<{{"]) partOneScore == 25137
-- True
--
-- >>> solve (Puzzle ["<{([{{}}[<[[[<>{}]]]>[]]"]) partTwoScore == 294
-- True
--
-- >>> solve (Puzzle ["[({(<(())[]>[[{[]{<()<>>"]) partTwoScore == 288957
-- True
--
-- >>> solve (Puzzle ["[(()[<>])]({[<{<<[]>>("]) partTwoScore == 5566
-- True
--
-- >>> solve (Puzzle ["(((({<>}<{<{<>}{[]{[]{}"]) partTwoScore == 1480781
-- True
--
-- >>> solve (Puzzle ["{<[[]]>}<{[{[{[]{()[[[]"]) partTwoScore == 995444
-- True
solve :: Puzzle -> Scorer Int -> Int
solve (Puzzle p) s = score s $ map go p
  where
    go :: String -> Either Char String
    go s = foldl go' (Right []) s

    go' :: Either Char String -> Char -> Either Char String
    go' (Left i) _ = Left i
    go' (Right []) c = Right [c]
    go' (Right stack@(h : t)) c
      | opening c = Right $ c : stack
      | closing h c = Right t
      | otherwise = Left c

-- Main
main :: IO ()
main = do
  putStrLn "Day Five"
  puzzle <- getPuzzle
  let Puzzle lines = puzzle
  print $ length lines
  putStr "Part one: "
  print $ solve puzzle partOneScore
  putStr "Part two: "
  print $ solve puzzle partTwoScore
