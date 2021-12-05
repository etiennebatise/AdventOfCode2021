module Util where

import Data.List
import Text.Parsec (ParseError, many1, parse, runP)
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

number :: Parser Int
number = read <$> many1 digit

unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Left e) = error $ show e
unsafeRight (Right r) = r

binToDec l = sum $ map (2 ^) $ findIndices (== 1) $ reverse l

charToInt :: Char -> Int
charToInt = read . (: "")
