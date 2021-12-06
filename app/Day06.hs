module Day06 where

import Data.Bool
import Data.List
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Util

newtype Puzzle = Puzzle [Int] deriving (Show)

puzzle :: Parser Puzzle
puzzle = Puzzle <$> number `sepBy` char ','

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/06.txt"

passDay :: Int -> [Int]
passDay 0 = [6, 8]
passDay i = [i - 1]

passDay' :: [Int] -> [Int]
passDay' l = passDay =<< l

naiveSolveParts :: Int -> [Int] -> Int
naiveSolveParts day = length . last . take (day + 1) . iterate passDay'

naiveSolvePartsAgain days l = length $ foldr (const passDay') l $ replicate days 0

data NewFish = NewFish Int Int Int Int Int Int Int Int Int deriving (Show)

countN (NewFish nEight nSeven nSix nFive nFour nThree nTwo nOne nZero) =
  nEight + nSeven + nSix + nFive + nFour + nThree + nTwo + nOne + nZero

data OldFish = OldFish Int Int Int Int Int Int Int deriving (Show)

countO (OldFish oSix oFive oFour oThree oTwo oOne oZero) =
  oSix + oFive + oFour + oThree + oTwo + oOne + oZero

solveParts :: Int -> OldFish -> NewFish -> Int
solveParts 0 old new = countO old + countN new
solveParts
  day
  (OldFish oSix oFive oFour oThree oTwo oOne oZero)
  (NewFish nEight nSeven nSix nFive nFour nThree nTwo nOne nZero) =
    let nextOld = OldFish (oZero + nZero) oSix oFive oFour oThree oTwo oOne
        nextNew = NewFish (oZero + nZero) nEight nSeven nSix nFive nFour nThree nTwo nOne
     in solveParts (day - 1) nextOld nextNew

countElem i l = length $ elemIndices i l

main :: IO ()
main = do
  putStrLn "Day Five"
  Puzzle ints <- getPuzzle
  let occ = flip countElem ints
  let oldFish = OldFish (occ 6) (occ 5) (occ 4) (occ 3) (occ 2) (occ 1) (occ 0)
  let newFish = NewFish 0 0 0 0 0 0 0 0 0
  putStr "Part one: "
  print $ naiveSolveParts 80 ints
  putStr "Part two: "
  print $ solveParts 256 oldFish newFish
