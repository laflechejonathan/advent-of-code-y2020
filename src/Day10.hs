module Day10 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List

getJoltageDeltas :: [Int] -> [Int]
getJoltageDeltas descendingJoltages =
  case descendingJoltages of
    [] -> error "no more joltages to process"
    [x] -> [x]
    x:y:rest -> (x - y) : getJoltageDeltas (y : rest)

compatibleAdapters :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
compatibleAdapters adapters current =
    let value = snd current in
    let result = [x | x <- adapters ++ [(0,0)], value - snd x > 0 && value - snd x <= 3] in
    trace ("current: " ++ show current ++ " compat: " ++ show result)  result

computePossibleArrangementsDP :: [Int] -> Int
computePossibleArrangementsDP adapters =
  let indexedAdapters  = zip [1..] adapters in
  let compat = compatibleAdapters indexedAdapters in
  let arrangements = 1:map (\(i, v) -> sum (map (\(idx, _) -> arrangements!!idx) (compat (i, v)))) indexedAdapters in
  arrangements!!(length arrangements - 1)


day10 :: [String] -> String
day10 lines = do
  let numbers = map (\l -> read l :: Int) lines
  show (computePossibleArrangementsDP (sort numbers))
