module Day9 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE

{-
  problem is to find the first number that is not a sum of two previous elements.
  We can start with a brute force solution - just compute all possible sums? let's see if that's fast enough

  [ x | x <- splits!!1, x /= '+'
-}

allPossibleSums :: [Int] -> Set.Set Int
allPossibleSums list =
  Set.fromList (concat [[firstNumber + secondNumber | secondNumber <- list, firstNumber /= secondNumber] | firstNumber <- list])

findViolatingNumber :: [Int] -> [Int] -> Int
findViolatingNumber numbers window
  | null numbers = error "found no violating number"
  | otherwise =
    let current = head numbers in
    if Set.member current (allPossibleSums window) then
      findViolatingNumber (drop 1 numbers) (drop 1 window ++ [current])
    else
      current

findContiguousSeries :: [Int] -> [Int] -> Int -> [Int]
findContiguousSeries numbers currentWindow target
  | currentSum == target = currentWindow
  | currentSum > target = findContiguousSeries numbers (drop 1 currentWindow) target
  | currentSum < target = findContiguousSeries (drop 1 numbers) (currentWindow ++ take 1 numbers) target
  where currentSum = sum currentWindow


day9 :: [String] -> String
day9 lines = do
  let numbers = map (\l -> read l :: Int) lines
  let windowSize = 25
  let violatingSum = findViolatingNumber (drop windowSize numbers) (take windowSize numbers)
  let series = findContiguousSeries numbers [] (trace (show "violating sum: " ++ show violatingSum) violatingSum)
  show (minimum series + maximum (trace (show series) series))