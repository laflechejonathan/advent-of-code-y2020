module Day5 where

import Debug.Trace
import Data.List

readBinary :: [Int] -> Int
readBinary [] = 0
readBinary (x:xs) = x + 2 * readBinary xs

rowToBinary :: String -> Int
rowToBinary str =
  readBinary (map (\c -> case c of
      'B' -> 1
      'F' -> 0
      _ -> error ("unexpected row character: " ++ str)) str)

colToBinary :: String -> Int
colToBinary str =
  readBinary (map (\c -> case c of
      'R' -> 1
      'L' -> 0
      _ -> error ("unexpected col character: " ++ str)) str)

seatId :: Int -> Int -> Int
seatId row col = row * 8 + col

-- BBFFBBFRLL
parseId :: String -> Int
parseId string =
  let row = rowToBinary (reverse (take 7 string)) in
  let col = colToBinary (reverse (drop 7 string)) in
  let id = seatId row col in
  trace ((show row) ++ ":" ++ (show col) ++ " = " ++ (show id)) id

findMySeat :: [Int] -> Int
findMySeat seats =
  case seats of
    a:b:others -> if (b - a) == 2 then (a + 1) else findMySeat ([b] ++ others)
    _ -> error ("could not find seat")

day5 :: [String] -> String
day5 lines = do
  show (findMySeat (sort (map parseId lines)))

