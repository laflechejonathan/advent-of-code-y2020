module Day8 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE

parseOperation :: String -> (String, Int)
parseOperation str =
  let splits = splitOn " " str in
  if length splits == 2 then
    (head splits, read [ x | x <- splits!!1, x /= '+' ] :: Int)
  else
    error ("Could not parse: " ++ str)

getAccumulatorValueAtFirstLoop :: [(String, Int)] -> Set.Set Int -> Int -> Int -> Int
getAccumulatorValueAtFirstLoop program visitedLines currentInstruction accumulatorValue
  |  Set.member currentInstruction visitedLines = accumulatorValue
  | currentInstruction >= length program = error "instruction pointer out of bounds"
  | otherwise =
    let newVisitedLines = Set.insert currentInstruction visitedLines in
    case program!!currentInstruction of
        ("nop", _) -> getAccumulatorValueAtFirstLoop program newVisitedLines (currentInstruction + 1) accumulatorValue
        ("acc", val) -> getAccumulatorValueAtFirstLoop program newVisitedLines (currentInstruction + 1) (accumulatorValue + val)
        ("jmp", val) -> getAccumulatorValueAtFirstLoop program newVisitedLines (currentInstruction + val) accumulatorValue
        (unknown, _) -> error ("unknown instruction" ++ unknown)


getAccumulatorValueCorrectingCorruption :: [(String, Int)] -> Set.Set Int -> Int -> Int -> Bool -> (Bool, Int)
getAccumulatorValueCorrectingCorruption program visitedLines currentInstruction accumulatorValue hasFlipped
  |  Set.member currentInstruction visitedLines = (False, accumulatorValue)
  | currentInstruction == length program = (True, accumulatorValue)
  | currentInstruction > length program = (False, accumulatorValue)
  | otherwise =
    let newVisitedLines = Set.insert currentInstruction visitedLines in
    case program!!currentInstruction of
        ("acc", val) -> getAccumulatorValueCorrectingCorruption program newVisitedLines (currentInstruction + 1) (accumulatorValue + val) hasFlipped
        (nopOrJump, val) ->
          if hasFlipped then
            case nopOrJump of
              "nop" -> getAccumulatorValueCorrectingCorruption program newVisitedLines (currentInstruction + 1) accumulatorValue hasFlipped
              "jmp" -> getAccumulatorValueCorrectingCorruption program newVisitedLines (currentInstruction + val) accumulatorValue hasFlipped
              unknown -> error ("unknown instruction" ++ unknown)
          else
            let nopResult = getAccumulatorValueCorrectingCorruption program newVisitedLines (currentInstruction + 1) accumulatorValue (nopOrJump /= "nop") in
            let jmpResult = getAccumulatorValueCorrectingCorruption program newVisitedLines (currentInstruction + val) accumulatorValue (nopOrJump /= "jmp") in
            let didFlip = (nopOrJump == "nop" && fst jmpResult) || (nopOrJump == "jmp" && fst nopResult) in
            let result = if fst nopResult then nopResult else jmpResult in
            trace ("instruction#: " ++ show currentInstruction ++  "result: " ++ show result ++ " did flip: " ++ show didFlip) result
{-
  nop +0  | 1
  acc +1  | 2, 8(!)
  jmp +4  | 3
  acc +3  | 6
  jmp -3  | 7
  acc -99 |
  acc +1  | 4
  jmp -4  | 5
  acc +6  |
-}
day8 :: [String] -> String
day8 lines = do
  let program = map parseOperation lines
  show (getAccumulatorValueCorrectingCorruption program Set.empty 0 0 False)