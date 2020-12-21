module Day13 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord


busIdWaitTime :: Int -> Int -> Int
busIdWaitTime earliestTime busId =
  let closestPossible = (earliestTime `div` busId) * busId in
  if closestPossible == earliestTime then 0 else closestPossible + busId - earliestTime

allTheSame :: [Int] -> Bool
allTheSame list =
  length (Set.fromList list) == 1

{-
    part2, given:

    7,13,x,x,59,x,31,19
    find T such that
     T      / 7  = 0
    (T + 1) / 13 = 0
    (T + 4) / 59 = 0
    (T + 6) / 31 = 0
    (T + 7) / 19 = 0

    1068781
-}
naivePart2 :: [(Int, Int)] -> Int -> Int -> Int
naivePart2 indexedBusIds timestamp step =
  case indexedBusIds of
    [] -> timestamp
    x:xs ->
      let busId = fst x in
      let busIndex = snd x in
      let newTimestamp = head [timestamp + step * i | i <- [0..], (timestamp + step * i + busIndex) `mod` busId == 0] in
      naivePart2 xs newTimestamp (step * busId)

solve :: [String] -> String
solve lines = do
  let earliestTimestamp = read (head lines) :: Int
  let busIds = [(read id :: Int, idx) | (id, idx) <- zip (splitOn "," (lines!!1)) [0..], id /= "x"]
  show (naivePart2 busIds 1 1)
