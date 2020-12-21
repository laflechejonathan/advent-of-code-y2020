module Day14 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

data Instruction = SetMask String | SetMemory Int Int deriving (Show)

{-
  mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
-}
parseMask :: String -> Instruction
parseMask line =
  let (_, _, _, matches) = line =~ "mask = (.*)" :: (String, String, String, [String]) in
  SetMask (head matches)

mem[8] = 11
parseMemory :: String -> Instruction
parseMemory line =
  let (_, _, _, matches) = line =~ "mem\\[([0-9]+)\\] = ([0-9]+)" :: (String, String, String, [String]) in
  SetMemory (read (head matches) :: Int) (read (matches!!1) :: Int)

parseLine :: String -> Instruction
parseLine line =
  if line =~ "mask.*" :: Bool then parseMask line else parseMemory line

permuteMask :: String -> String -> [String]
permuteMask mask bitsSoFar =
  case mask of
    [] -> [bitsSoFar]
    'X':xs -> permuteMask xs (bitsSoFar ++ "0") ++ permuteMask xs (bitsSoFar ++ "1")
    x:xs -> permuteMask xs (bitsSoFar ++ [x])

mergeMask :: String -> String -> String
mergeMask binaryValue binaryMask =
  [case m of
    'X' -> 'X'
    '1' -> '1'
    '0' -> b | (b, m) <- zip binaryValue binaryMask]

intToBinaryString :: Int -> String -> String
intToBinaryString number stringSoFar
  | length stringSoFar == 36 = stringSoFar
  | otherwise =
      let possible = 2^(35 - length stringSoFar) in
      let newNumber = if possible <= number then number - possible else number in
      let newString = if possible <= number then stringSoFar ++ "1" else stringSoFar ++ "0" in
      intToBinaryString newNumber newString

binaryToInt :: String -> Int
binaryToInt bitList =
  sum [if b == '1' then 2^exp else 0 | (b, exp) <- zip bitList (reverse [0..(length bitList - 1)])]

applyMask :: Int -> String -> Int
applyMask value mask =
  let andMask = binaryToInt [if c == '0' then '0' else '1' | c <- mask] in
  let orMask = binaryToInt [if c == '1' then '1' else '0' | c <- mask] in
  (value .|. orMask) .&. andMask

initialize :: [String] -> String -> Map.Map Int Int -> Map.Map Int Int
initialize instructions activeMask memory =
  case instructions of
    [] -> memory
    ins:others -> case parseLine ins of
              SetMask mask -> initialize others mask memory
              SetMemory address value -> initialize others activeMask (Map.insert address (applyMask value activeMask) memory)

applyMemory :: String -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int
applyMemory mask address value memory =
  let effectiveMask = mergeMask (intToBinaryString address "") mask in
  Map.union (Map.fromList [(addr, value) | addr <- map binaryToInt (permuteMask effectiveMask "")]) memory

initialize2 :: [String] -> String -> Map.Map Int Int -> Map.Map Int Int
initialize2 instructions activeMask memory =
  case instructions of
    [] -> memory
    ins:others -> case parseLine ins of
              SetMask mask -> initialize2 others mask memory
              SetMemory address value -> initialize2 others activeMask (applyMemory activeMask address value memory)

solve :: [String] -> String
solve lines = do
  {-
    part 1
    let memory = initialize lines "" Map.empty
    show $ sum (Map.elems (trace (show memory) memory))
   -}
  let memory = initialize2 lines "" Map.empty
  show $ sum (Map.elems (trace (show memory) memory))
