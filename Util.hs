module Util where

import Text.Parsec (many1, runP, ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)

number :: Parser Int
number = read <$> many1 digit