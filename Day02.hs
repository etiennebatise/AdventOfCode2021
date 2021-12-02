{-# LANGUAGE LambdaCase #-}

module Day02 (main) where

import Control.Applicative
import Text.Parsec (many1, runP, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, spaces, digit)

-- Command
data Command = Horizontal Int | Vertical Int deriving (Show)

command :: Parser Command 
command =  forward <|> up <|> down
  where
    forward = string "forward" *> (Horizontal <$> number)
    up = string "up" *> (Vertical . negate <$> number)
    down = string "down" *> (Vertical <$> number)
    number = spaces *> (read <$> many1 digit)

-- Puzzle
type Puzzle = [Command]

getPuzzle :: IO (Either ParseError Puzzle)
getPuzzle = traverse parse . lines <$> readFile "./02.txt"
  where
    parse = runP command () ""

-- Solver 
type Handler a = a -> Command -> a

partOneHandler :: Handler (Int, Int)
partOneHandler (v, h) = \case
  Horizontal i -> (v, h + i)
  Vertical i -> (v + i, h)

partTwoHandler :: Handler (Int, Int, Int)
partTwoHandler (v, h, a) = \case
  Horizontal i -> (v + a * i, h + i, a)
  Vertical i -> (v, h, a + i)

solve :: Handler a -> a -> Puzzle -> a
solve = foldl -- I wonder how the handler would look in foldr

main :: IO ()
main = do
  putStrLn "Day Two"
  Right puzzle <- getPuzzle -- unsafe, who cares
  putStr "Part one: "
  print $ uncurry (*) $ solve partOneHandler (0, 0) puzzle
  putStr "Part two: "
  print $ (\(a, b, _) -> a*b) $ solve partTwoHandler (0, 0, 0) puzzle


