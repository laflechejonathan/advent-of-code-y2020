module Day15 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

-- keep shift register of 2 last turns each number was spoken
type Memory = Map.Map Int (Maybe Int, Maybe Int)

updateMemory :: Memory -> Int -> Int -> Memory
updateMemory memory numberSpoken turnNumber =
  let maybeSecond = (snd =<< Map.lookup numberSpoken memory) in
  Map.insert numberSpoken (maybeSecond, Just turnNumber) memory

queryMemory :: Memory -> Int -> Int
queryMemory memory numberSpoken =
  let maybeFirst = (fst =<< Map.lookup numberSpoken memory) in
  let maybeSecond = (snd =<< Map.lookup numberSpoken memory) in
  case maybeFirst of
    Nothing -> 0
    Just x -> case maybeSecond of
                Nothing -> 0
                Just y -> y - x

findNumber :: Int -> Int -> Int -> Memory -> Int
findNumber lastSpokenNumber turnNumber targetTurnNumber memory
  | turnNumber == targetTurnNumber + 1 = lastSpokenNumber
  | otherwise =
    let newSpokenNumber = queryMemory memory lastSpokenNumber in
    findNumber newSpokenNumber (turnNumber + 1) targetTurnNumber (updateMemory memory newSpokenNumber turnNumber)

solve :: [String] -> String
solve lines = do
  let numbers = [(read val :: Int, turn) | (turn, val) <- zip [1..] (splitOn "," (head lines))]
  let memory = foldl (\memory spokenNumber -> uncurry (updateMemory memory) spokenNumber) Map.empty numbers
  let lastNumber = fst (last numbers)
  let nextTurnNumber = snd (last numbers) + 1
  show (findNumber lastNumber nextTurnNumber 30000000 memory)

