module Day18 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

tokenize :: String -> String -> [String] -> [String]
tokenize string inProgressToken tokens =
  let flushedToken = if null inProgressToken then tokens else tokens ++ [inProgressToken] in
  case string of
    [] -> flushedToken
    '+':xs -> tokenize xs "" (tokens ++ ["+"])
    '*':xs -> tokenize xs "" (tokens ++ ["*"])
    '(':xs -> tokenize xs "" (tokens ++ ["("])
    ')':xs -> tokenize xs "" (flushedToken ++ [")"])
    ' ':xs -> tokenize xs "" flushedToken
    char:xs -> tokenize xs (inProgressToken ++ [char]) tokens

data Node = Leaf String | Nested [Node] deriving ( Show )

createComputationTree :: [String] -> [Node] -> ([Node], [String])
createComputationTree tokens acc
  | null tokens = (acc, [])
  | otherwise =
     let curr = head tokens in
     let rest = tail tokens in
     case curr of
       "(" ->
         let (nodes, afterClosed) = createComputationTree rest [] in
         createComputationTree afterClosed (acc ++ [Nested nodes])
       ")" -> (acc, rest)
       other -> createComputationTree rest (acc ++ [Leaf other])

processOperations :: [String] -> [String]
processOperations operations
  | length operations == 3 =
    let a = read (operations!!0) :: Int in
    let b = read (operations!!2) :: Int in
    case operations!!1 of
      "*" -> [show (a * b)]
      "+" -> [show (a + b)]
      unknown -> error ("unknown operator: " ++ unknown)
  | otherwise = operations

computeRec :: [Node] -> [String] -> String
computeRec allNodes operands =
  let newOperands = processOperations operands in
  case allNodes of
    [] -> head newOperands
    x:xs ->
      case x of
        Leaf leaf -> computeRec xs (newOperands ++ [leaf])
        Nested nodes -> computeRec xs (newOperands ++ [computeRec nodes []])

processAdditions :: [String] -> [String]
processAdditions operations
  | length operations >= 3 =
    let others = (reverse . drop 3 . reverse) operations in
    let realOps = drop (length operations - 3) operations in
    let a = read (realOps!!0) :: Int in
    let b = read (realOps!!2) :: Int in
    if realOps!!1 == "+" then others ++ [show (a  + b)] else operations
  | otherwise = operations

processMultiplications :: [String] -> [String]
processMultiplications operations
  | length operations >= 3 =
    let others = (reverse . drop 3 . reverse) operations in
    let realOps = drop (length operations - 3) operations in
    let a = read (realOps!!0) :: Int in
    let b = read (realOps!!2) :: Int in
    if realOps!!1 == "*" then others ++ [show (a  * b)] else operations
  | otherwise = operations

computePrecedenceRec :: [Node] -> [String] -> [String]
computePrecedenceRec allNodes operands =
  let newOperands = processAdditions operands in
  let postAdditionOperands = (case allNodes of
                                [] -> newOperands
                                x:xs ->
                                  case x of
                                    Leaf leaf -> computePrecedenceRec xs (newOperands ++ [leaf])
                                    Nested nodes -> computePrecedenceRec xs (newOperands ++ computePrecedenceRec nodes [])) in
  processMultiplications postAdditionOperands

compute :: String -> Int
compute line =
  let (tree, _) = createComputationTree (tokenize line "" []) [] in
  read (head (computePrecedenceRec tree [])) :: Int

solve :: [String] -> String
solve lines = do
  show (sum (map compute lines))


