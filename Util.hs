module Util where

import Data.List
import Text.Parsec (many1, runP, ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)

number :: Parser Int
number = read <$> many1 digit

unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Left e) = error $ show e
unsafeRight (Right r) = r

binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l