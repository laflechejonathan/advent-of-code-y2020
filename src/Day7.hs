{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.Split
import Text.Regex.PCRE

{-
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.

  becomes:
  { (plum) -> [(5, faded blue), (6, dotted black)
-}

bagColor :: T.Text -> T.Text
bagColor txt = T.strip (T.replace "." "" (T.replace "bag" "" (T.replace " bags" "" txt)))

parseBagChild :: T.Text -> (Int, T.Text)
parseBagChild bagChild =
  let regex = " ?([0-9]+) (.*)" :: String in
  let bagChildStr = T.unpack bagChild in
  let (_, _, _, matches) = bagChildStr =~ regex :: (String, String, String, [String]) in
  if length matches == 2 then
    let count = read (head matches) :: Int in
    (count, bagColor (T.pack (matches!!1)))
  else
    error (T.unpack ("regex did not match: " <> bagChild))


splitInTwo :: T.Text -> T.Text -> (T.Text, T.Text)
splitInTwo text split =
  let result = T.splitOn split text in
  if length result == 2 then (result!!0, result!!1) else error (T.unpack ("cannot split in two: " <> text))

parseLine :: T.Text -> (T.Text, [(Int, T.Text)])
parseLine line =
  let (key, values) = splitInTwo line " contain " in
  if values == "no other bags." then
    (bagColor key, [])
  else
    (bagColor key, map parseBagChild (T.splitOn "," values))

canContainColorRecursively :: Map.Map T.Text [(Int, T.Text)] -> T.Text -> T.Text -> Bool
canContainColorRecursively colorRules bagColor targetColor =
  bagColor == targetColor ||
    case Map.lookup bagColor colorRules of
      Just values -> any (\v -> canContainColorRecursively colorRules (snd v) targetColor) values
      Nothing -> False

countBagsRecursively :: Map.Map T.Text [(Int, T.Text)] -> T.Text -> Int
countBagsRecursively colorRules bagColor =
    case Map.lookup bagColor colorRules of
      Just values -> sum (map (\v -> (fst v) + (fst v) * (countBagsRecursively colorRules (snd v))) values)
      -- Just values -> sum (map fst values)
      Nothing -> 0


day7 :: [String] -> String
day7 lines = do
  let bagRules = Map.fromList (map (parseLine . T.pack) lines)
  show (countBagsRecursively bagRules "shiny gold")
  -- let bagColors = Map.keys bagRules
  -- show (length (filter id (map (\key -> canContainColorRecursively bagRules key "shiny gold") bagColors))) - 1
