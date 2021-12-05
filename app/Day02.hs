{-# LANGUAGE LambdaCase #-}

module Day02 (main) where

import Control.Applicative ((<|>))
import Text.Parsec (many1, runP, ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, spaces, digit)
import Util (number)

-- Command
data Command = Horizontal Int | Vertical Int deriving (Show)

command :: Parser Command 
command =  forward <|> up <|> down
  where
    forward = commandParser "forward" Horizontal
    up      = commandParser "up"     (Vertical . negate)
    down    = commandParser "down"    Vertical
    commandParser token f = string token *> spaces *> (f <$> number)

-- Puzzle
type Puzzle = [Command]

getPuzzle :: IO (Either ParseError Puzzle)
getPuzzle = traverse (parse command "") . lines <$> readFile "./assets/02.txt"
  
-- Solver 
type Interpreter state = state -> Command -> state

partOne :: Interpreter (Int, Int)
partOne (v, h) = \case
  Horizontal i -> (v, h + i)
  Vertical i -> (v + i, h)

partTwo :: Interpreter (Int, Int, Int)
partTwo (v, h, a) = \case
  Horizontal i -> (v + a * i, h + i, a)
  Vertical i -> (v, h, a + i)

solve :: Interpreter a -> a -> Puzzle -> a
solve = foldl -- Yeah.. I also mixed up foldr and foldl

main :: IO ()
main = do
  putStrLn "Day Two"
  Right puzzle <- getPuzzle 
  -- ^ unsafe, who cares
  putStr "Part one: "
  print $ uncurry (*) $ solve partOne (0, 0) puzzle
  putStr "Part two: "
  print $ (\(v, h, _) -> v*h) $ solve partTwo (0, 0, 0) puzzle


