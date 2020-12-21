module Day19 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

data Pattern = Leaf Char | Sequence [Pattern] | Or (Pattern, Pattern) deriving (Show)

orRegex = "(.*) \\| (.*)"
leafRegex = "\"([a-z])\""

parseRule :: [Pattern] -> String -> Pattern
parseRule ruleList rule
  | rule =~ orRegex :: Bool =
      let (_, _, _, matches) = rule =~ orRegex :: (String, String, String, [String]) in
      Or (parseRule ruleList (matches!!0), parseRule ruleList (matches !!1))
  | rule =~ leafRegex :: Bool =
      let (_, _, _, matches) = rule =~ leafRegex :: (String, String, String, [String]) in
      Leaf (head (head matches))
  | otherwise =
      Sequence [ruleList!!(read val :: Int) | val <- splitOn " " rule]

ruleTuple :: String -> (Int, String)
ruleTuple line =
  let (_, _, _, matches) = line =~ "([0-9]+): (.*)" :: (String, String, String, [String]) in
  let index = read (matches!!0) :: Int in
  (index, matches!!1)

just maybe =
  case maybe of
    Just x -> x
    Nothing -> error "expected value"

matchesRec :: String -> [Pattern] -> (Bool, String)
matchesRec string patternsToMatch
  | null patternsToMatch = (True, string)
  | null string = (False, string)
  | otherwise =
      let (doesMatch, rmd) = (case head patternsToMatch of
                                Sequence seq ->
                                  let (seqMatches, seqRmd) = matchesRec string seq in
                                  if seqMatches then (True, seqRmd) else (False, string)
                                Or (a, b) ->
                                  let (aMatches, aRmd) = matchesRec string [a] in
                                  let (bMatches, bRmd) = matchesRec string [b] in
                                  (if aMatches then
                                      (True, aRmd)
                                   else if bMatches then
                                      (True, bRmd)
                                   else
                                      (False, string))
                                Leaf c ->
                                  if c == head string then (True, (tail string)) else (False, string)) in
      if doesMatch then matchesRec rmd (tail patternsToMatch) else (False, string)

matches :: String -> Pattern -> Bool
matches string pattern =
  let (stringMatches, remainder) = matchesRec string [pattern] in
  stringMatches && null remainder

ruleZeroWithOverrides :: Map.Map Int String -> Pattern -> Pattern -> Pattern
ruleZeroWithOverrides ruleMap overrideEight overrideEleven =
  let ruleList = [if i == 8 then overrideEight else (if i == 11 then overrideEleven else parseRule ruleList (just (Map.lookup i ruleMap))) | i <- [0..(maximum (Map.keys ruleMap))]] in
  ruleList!!0

solve lines = do
  let rules = takeWhile (not . null) lines
  let strings = tail (dropWhile (not . null) lines)
  let ruleMap = Map.fromList(map ruleTuple rules)
  let ruleList = [parseRule ruleList (just (Map.lookup i ruleMap)) | i <- [0..(maximum (Map.keys ruleMap))]]
  let ruleOverrides = [(Sequence (replicate i (ruleList!!42)), Sequence (replicate j (ruleList!!42) ++ (replicate j (ruleList!!31)))) | i <- [1..30], j <- [1..30]]
  show $ length (Set.fromList [s | (o8, o11) <- ruleOverrides, s <- strings, matches s (ruleZeroWithOverrides ruleMap o8 o11)])