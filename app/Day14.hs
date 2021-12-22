-- |
module Day14 where

import Control.Monad.State (MonadState (put), State, evalState, get, join, replicateM, runState, state)
import Data.Array.ST (runSTUArray)
import Data.Bifunctor (Bifunctor (second))
import Data.Bits (Bits (bit))
import Data.Function (on)
import Data.Functor (($>))
import Data.List (sortOn)
import Data.List.Extra (group, groupSort, sort, sortBy)
import Data.Map (Map, adjust, alter, foldrWithKey', fromList, keys, toList, (!))
import Data.Monoid (Sum (getSum), mconcat, mempty)
import Text.Parsec (count, letter, many, many1, newline, parse, string)
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
  let zipped = zipWith (\a b -> ((a, b), 1 :: Sum Int)) s (tail s)
      mapped = map (second mconcat) $ groupSort $ (zipped ++ [((last s, ' '), 1)])
   in fromList mapped

puzzle :: Parser Puzzle
puzzle = do
  polymerTemplate <- polymerFromString <$> many letter
  newline
  newline
  rules <- fromList <$> many1 (rule <* newline)
  return $ Puzzle polymerTemplate rules

getPuzzle :: IO Puzzle
getPuzzle = unsafeRight . parse puzzle "" <$> readFile "./assets/14.txt"

computeQuantities :: PolymerTemplate -> Int
computeQuantities polymer = getSum $ mostCommon <> negate leastCommon
  where
    x = sortOn snd $ map (second mconcat) $ groupSort $ uncurry ($>) <$> toList polymer
    leastCommon = snd $ head x
    mostCommon = snd $ last x

runStep :: PairInsertionRules -> State PolymerTemplate Int
runStep rules = state go
  where
    go :: PolymerTemplate -> (Int, PolymerTemplate)
    go polymer =
      let newPolymer = foldrWithKey' processPair polymer polymer
       in (computeQuantities newPolymer, newPolymer)

    processPair :: (Char, Char) -> Sum Int -> PolymerTemplate -> PolymerTemplate
    processPair (a, ' ') _ polymer = polymer
    processPair pair count polymer =
      let newPairs = subPairs pair
       in decrease count pair $ foldr (upsert count) polymer newPairs

    subPairs t@(a, b) = [(a, ruleFor t), (ruleFor t, b)]
    ruleFor t = rules ! t

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
