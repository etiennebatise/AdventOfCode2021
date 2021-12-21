-- |
module Day12 where

import Data.Functor (($>))
import Data.List.Extra (group, groupSort, sort)
import Data.Map (Map, fromList, (!))
import Data.Tuple (swap)
import Debug.Trace
import Text.Parsec (char, lower, many1, newline, parse, sepBy, string, try, upper, (<|>))
import Text.Parsec.String (Parser)
import Util (unsafeRight)

data Cave = Start | End | Big String | Small String deriving (Show, Eq, Ord)

isBig (Big _) = True
isBig _ = False

isSmall (Small _) = True
isSmall _ = False

isEnd End = True
isEnd _ = False

newtype Puzzle = Puzzle (Map Cave [Cave]) deriving (Show)

cave :: Parser Cave
cave = try start <|> try end <|> big <|> small
  where
    start = string "start" $> Start
    end = string "end" $> End
    big = Big <$> many1 upper
    small = Small <$> many1 lower

mapEntry :: Parser (Cave, Cave)
mapEntry = (,) <$> cave <* char '-' <*> cave

puzzle :: Parser Puzzle
puzzle = go <$> many1 (mapEntry <* try newline)
  where
    go xs = Puzzle $ fromList $ groupSort $ xs ++ map swap xs

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/12.txt"

-- Part one
type VisitingRule = [Cave] -> Cave -> Bool

partOne :: VisitingRule
partOne path cave = isBig cave || isEnd cave || (isSmall cave && cave `notElem` path)

partTwo :: VisitingRule
partTwo path cave = isBig cave || isEnd cave || (isSmall cave && (cave `notElem` path || smallCavesVisitedAtMostOnce))
  where
    smallCavesVisitedAtMostOnce = all ((< 2) . length) . group . sort . filter isSmall $ path

solve :: VisitingRule -> Puzzle -> Int
solve visitable (Puzzle plan) = go [] Start
  where
    go :: [Cave] -> Cave -> Int
    go l End = 1
    go path current = case filter (visitable (current : path)) (plan ! current) of
      [] -> 0
      xs -> sum $ map (go (current : path)) xs

main :: IO ()
main = do
  putStrLn "Day Twelve"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ solve partOne puzzle
  putStr "Part two: "
  print $ solve partTwo puzzle
