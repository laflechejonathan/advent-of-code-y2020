module Day6 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map

batchAnswers :: [String] -> [String] -> [[String]]
batchAnswers lines batchInProgress =
  case lines of
    [] -> [batchInProgress]
    line:otherLines -> if (length line) == 0
        then [batchInProgress] ++ (batchAnswers otherLines [])
        else (batchAnswers otherLines (batchInProgress ++ [line]))

countAnswersWithAllYes :: [String] -> Int
countAnswersWithAllYes answers =
  let peopleCount = length answers in
  let answerCounts = (Map.toList (Map.fromListWith (+) [(c, 1) | c <- (concat answers)])) in
  length (filter (\(_, count) -> count == peopleCount) (trace (show answerCounts) answerCounts))

day6 :: [String] -> String
day6 lines = do
  let answerCounts = map countAnswersWithAllYes (batchAnswers lines [])
  let sum = foldl (+) 0 answerCounts
  show sum