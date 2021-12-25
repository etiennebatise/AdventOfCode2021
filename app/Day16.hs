{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
module Day16 where

import Control.Monad (join, void, when)
import Data.Bool
import Data.Function ((&))
import Debug.Trace
import GHC.Base (VecElem (Int16ElemRep))
import Text.Parsec (Column, SourcePos, alphaNum, char, count, digit, getPosition, letter, many, many1, manyTill, parse, sourceColumn, try, unexpected)
import Text.Parsec.String (Parser)
import Util (unsafeRight)

hexToBinary :: Char -> String
hexToBinary '0' = "0000"
hexToBinary '1' = "0001"
hexToBinary '2' = "0010"
hexToBinary '3' = "0011"
hexToBinary '4' = "0100"
hexToBinary '5' = "0101"
hexToBinary '6' = "0110"
hexToBinary '7' = "0111"
hexToBinary '8' = "1000"
hexToBinary '9' = "1001"
hexToBinary 'A' = "1010"
hexToBinary 'B' = "1011"
hexToBinary 'C' = "1100"
hexToBinary 'D' = "1101"
hexToBinary 'E' = "1110"
hexToBinary 'F' = "1111"

type Puzzle = String

puzzle :: Parser Puzzle
puzzle = many1 alphaNum

getPuzzle :: IO String
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/16.txt"

-- Lib
binToDec :: Integral i => i -> i
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + mod i 10

version' :: Parser Int
version' = binToDec . read <$> count 3 digit

typeId' :: Parser Int
typeId' = binToDec . read <$> count 3 digit

readBinary :: String -> Int
readBinary = binToDec . read

fourBits :: Parser String
fourBits = count 4 digit

-- |
-- >>> parse literal "" "101111111000101"
-- NOW Right 2021
literal :: Parser Int
literal = (\l i -> readBinary $ l ++ i) <$> numberInit <*> numberLast
  where
    -- leadingZeros = many zero
    numberInit = join <$> many (one *> fourBits)
    numberLast = zero *> fourBits
    one = char '1'
    zero = char '0'

lengthTypeId :: Parser Int
lengthTypeId = read . (: []) <$> digit

data Header = Header
  { version :: Int,
    typeId :: Int
  }
  deriving (Show)

data Packet
  = Literal
      { header :: Header,
        value :: Int
      }
  | Operator
      { header :: Header,
        lengthType :: Int,
        packets :: [Packet]
      }
  deriving (Show)

-- |
-- >>> parse operator "" "00000000000110111101000101001010010001001000000000"
-- Right 8
--
-- >>> parse operator "" "10000000001101010000001100100000100011000001100000"
-- Right 7
operator :: Parser [Int]
operator = do
  ltid <- lengthTypeId
  length <-
    if ltid == 0
      then readBinary <$> count 15 digit
      else readBinary <$> count 11 digit
  pos <- sourceColumn <$> getPosition
  packets <-
    if ltid == 0
      then manyTill (packet False) (hasParsed (pos + length))
      else count length (packet False)
  pure $ packets
  where
    hasParsed maxColumn = (bool (fail "foo") (pure ()) . (== maxColumn)) . sourceColumn =<< getPosition

-- |
--
-- >>> parse (packet True) "" "110100101111111000101000"

-- >>> parse (packet True) "" "11010001010"

-- >>> parse (packet True) "" "010100100010010000000"

-- >>> parse (packet True) "" "100010100000000001001010100000000001101010000000000000101111010001111000"
packet :: Bool -> Parser Int
packet pad = do
  initPos <- sourceColumn <$> getPosition
  v <- version'
  tid <- typeId'
  let isLiteral = tid == 4
  p <- if isLiteral then literal else op tid <$> operator
  lastPos <- sourceColumn <$> getPosition
  when pad $ void $ trailingZeros lastPos initPos
  return p
  where
    trailingZeros lastPos initPos = count (padding (lastPos - initPos + 1) 4) digit
    padding value base = case value `mod` base of
      0 -> 0
      n -> base - n

    op = \case
      0 -> sum
      1 -> product
      2 -> minimum
      3 -> maximum
      5 -> \[f, s] -> if f > s then 1 else 0
      6 -> \[f, s] -> if f < s then 1 else 0
      7 -> \[f, s] -> if f == s then 1 else 0

-- |
-- >>> puzzleToBinaryString "8A004A801A8002F478"
-- "100010100000000001001010100000000001101010000000000000101111010001111000"
puzzleToBinaryString = (=<<) hexToBinary

debug s v = trace (s ++ " " ++ show v) v

-- |
-- >>> partOne "A0016C880162017C3686B18A3D4780"
-- WAS WAS Right 12
-- WAS NOW Right 23
-- NOW Right 31
partOne :: Puzzle -> Int
partOne = unsafeRight . parse (packet True) ""

partTwo :: Packet -> Int
partTwo (Literal _ value) = debug "LITERAL" value
partTwo (Operator (Header _ typeId) _ packets) =
  map partTwo packets
    & case typeId of
      0 -> sum
      1 -> product
      2 -> minimum
      3 -> maximum
      5 -> \[f, s] -> if f > s then 1 else 0
      6 -> \[f, s] -> if f < s then 1 else 0
      7 -> \[f, s] -> if f == s then 1 else 0

main :: IO ()
main = do
  putStrLn "Day Sixteen"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ partOne puzzle
  putStr "Part two: doesn't work "
  print $ partTwo puzzle
