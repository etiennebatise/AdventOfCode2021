-- |
module Day14 where

import Control.Monad.State (State, evalState, replicateM, state)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Functor (($>))
import Data.List.Extra (groupSort)
import Data.Map (Map, adjust, alter, foldrWithKey', fromList, toList, (!))
import Data.Monoid (Sum (getSum), mconcat, mempty)
import Data.Tuple.Extra (dupe)
import Text.Parsec (count, letter, many1, newline, parse, string)
import Text.Parsec.String (Parser)
import Util (unsafeRight)

type Pair = (Char, Char)

type PolymerTemplate = Map Pair (Sum Int)

type PairInsertionRules = Map Pair Char

data Puzzle = Puzzle PolymerTemplate PairInsertionRules deriving (Show)

pair :: Parser Pair
pair = (,) <$> letter <*> letter

rule :: Parser (Pair, Char)
rule = (,) <$> (pair <* string " -> ") <*> letter

polymerFromString :: String -> PolymerTemplate
polymerFromString s =
  let zipped = zipWith (\a b -> ((a, b), 1)) s (tail s)
      lastElement = ((last s, ' '), 1)
      mapped = second mconcat <$> groupSort (lastElement : zipped)
   in fromList mapped

puzzle :: Parser Puzzle
puzzle = do
  polymerTemplate <- polymerFromString <$> many1 letter
  newline
  newline
  rules <- fromList <$> many1 (rule <* newline)
  return $ Puzzle polymerTemplate rules

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/14.txt"

computeQuantities :: PolymerTemplate -> Int
computeQuantities polymer = mostCommon - leastCommon
  where
    mostCommon = maximum occurences
    leastCommon = minimum occurences
    occurences = getSum . mconcat . snd <$> groupSort (mapOccurrences <$> toList polymer)
    mapOccurrences = uncurry ($>)

runStep :: PairInsertionRules -> State PolymerTemplate Int
runStep rules = state go
  where
    go = first computeQuantities . dupe . processPolymer

    processPolymer polymer = foldrWithKey' processPair polymer polymer

    processPair :: (Char, Char) -> Sum Int -> PolymerTemplate -> PolymerTemplate
    processPair (a, ' ') _ polymer = polymer
    processPair pair count polymer =
      decrease count pair $ foldr (upsert count) polymer $ subPairs pair

    subPairs pair@(a, b) = [(a, rules ! pair), (rules ! pair, b)]

upsert :: Sum Int -> (Char, Char) -> PolymerTemplate -> PolymerTemplate
upsert i = alter (Just . maybe i (<> i))

decrease :: Sum Int -> (Char, Char) -> PolymerTemplate -> PolymerTemplate
decrease i = adjust (<> negate i)

solve :: Int -> Puzzle -> Int
solve i (Puzzle polymer rules) = last $ evalState (replicateM i (runStep rules)) polymer

-- This one I hacked a bit
--
-- The polymer is stored in a Map (eg: dictonnary) where the keys are pairs holding the
-- number of occurences of the pair in the polymer
--
-- eg : NNCB is stored as { NN: 1, NC: 1, CB: 1 }
--
-- Then for all the keys, you create the new pairs holding the same counter as the
-- parent. You also decrease the parent counter by the same amount
--
-- { NC: 1, CN: 1, NB: 1, BC: 1, CH: 1, HB: 1, NN: 0, NC: 0, CB: 0 }
--
-- I do that n times thanks to the magic of State monad and replicateM
--
-- To count single letter occurences in the final polymer string, you actually
-- only need the count occurences of the first letter of each key in the
-- structure. To convince you, I'll just say that second letter of each pair is
-- the first one in another pair.
--
-- Yet, I lied. The rule above is not entirely true. It misses the last character of
-- the inital polymer string. It needs to be accounted for.
--
-- That is why I add another entry to the initial polymer structure :
--
-- { NN: 1, NC: 1, CB: 1, 'B '}
--
-- This means that ' ' (space) is a reserved character that cannot be used in
-- the rules. This character indicates that this entry is the last character of
-- the polymer and therefore, it should not be split.

main :: IO ()
main = do
  putStrLn "Day Fourteen"
  puzzle <- getPuzzle
  putStr "Part one: "
  print $ solve 10 puzzle
  putStrLn "Part two: "
  print $ solve 40 puzzle
