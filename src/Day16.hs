module Day16 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

ruleRegex = "(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"

{-
  e.g.
  seat: 13-40 or 45-50
-}
parseRule :: String -> (String, Int -> Bool)
parseRule line =
  let (_, _, _, matches) = line =~ ruleRegex :: (String, String, String, [String]) in
  let ruleName = head matches in
  let min1 = read (matches!!1) :: Int in
  let max1 = read (matches!!2) :: Int in
  let min2 = read (matches!!3) :: Int in
  let max2 = read (matches!!4) :: Int in
  (ruleName, \val -> (val >= min1 && val <= max1) || (val >= min2 && val <= max2))

parseRules :: [String] -> Map.Map String (Int -> Bool)
parseRules lines =
  let ruleLines = takeWhile (\l -> l =~ ruleRegex :: Bool) lines in
  Map.fromList (map parseRule ruleLines)


isValidTicket :: [Int] -> [Int -> Bool] -> Bool
isValidTicket values rules =
  and [any (\f -> f value) rules | value <- values]

{-
  nearby tickets:
  7,3,47
  ...
-}
getNearbyTickets :: [String] -> [Int -> Bool] -> [[Int]]
getNearbyTickets lines rules =
  let nearby = tail (dropWhile (/= "nearby tickets:") lines) in
  let tickets = [[read value :: Int | value <- splitOn "," line] | line <- nearby] in
  filter (`isValidTicket` rules) tickets

matchesAll :: [[Int]] -> (Int -> Bool) -> Int -> Bool
matchesAll tickets rule fieldIndex =
  and [rule (t!!fieldIndex) | t <- tickets]

reduceSolution :: Map.Map String [Int] -> Map.Map String Int -> Map.Map String Int
reduceSolution possible certain
  | null possible = certain
  | otherwise =
      let (certainRule, certainField) = head [(r, head f) | (r, f) <- Map.assocs possible, length f == 1] in
      let stillInPlay = Map.fromList [(r, filter (/= certainField) f) | (r, f) <- Map.assocs possible, r /= certainRule] in
      reduceSolution stillInPlay (Map.insert certainRule certainField certain)

solve :: [String] -> String
solve lines = do
  let ruleMap = parseRules lines
  let rules = Map.elems (trace (show (Map.keys ruleMap)) ruleMap)
  let validTickets = getNearbyTickets lines rules
  let numFields = length (head validTickets)
  let possibleSolutions = [(name, [i]) | (name, rule) <- Map.assocs ruleMap, i <- [0..numFields -  1], matchesAll validTickets rule i]
  let fieldIndexes = reduceSolution (Map.fromListWith (++) possibleSolutions) Map.empty
  let myTicket = [103,197,83,101,109,181,61,157,199,137,97,179,151,89,211,59,139,149,53,107]
  show $ product [myTicket!!index | (field, index) <- Map.assocs fieldIndexes, "departure" `isInfixOf` field]
