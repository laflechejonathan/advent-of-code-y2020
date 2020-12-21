module Day1 where

import qualified Data.Set as Set
import qualified Data.Map as Map


data Couple = Couple Integer Integer deriving (Show)
data Triple = Triple Integer Integer Integer deriving (Show)
missingThird :: Couple -> Integer
missingThird (Couple first second) =
  2020 - first - second

extractTriple :: Integer -> Map.Map Integer Couple -> Triple
extractTriple num map =
   case Map.lookup num map of
      Just (Couple x y) -> Triple x y num
      Nothing -> Triple 0 0 0

answer :: Triple -> Integer
answer (Triple a b c) = a * b * c

main :: IO ()
main = do
  content <- readFile ("./data/day1.txt")
  let numbers = map (\l -> read l :: Integer) (lines content)
  let couples = numbers >>= \n -> (map (\other -> (Couple n other)) (filter (/= n) numbers))
  let couplesMap = Map.fromList (map (\c -> (missingThird c, c)) couples)
  let triple = extractTriple (head (filter (\n -> Map.member n couplesMap) numbers)) couplesMap
  print (triple, (answer triple))
  {-
    let numberSet = Set.fromList numbers
    let numbersSumming = (filter (\n -> Set.member (2020 - n) numberSet) numbers)
    let first = head numbersSumming
    let second = 2020 - first
    print (first * second)
  -}
