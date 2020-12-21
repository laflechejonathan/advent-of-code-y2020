module Day11 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List


findFirstSeat :: [String] -> Int -> Int -> Int -> Int -> Char
findFirstSeat grid x y dx dy =
  case dropWhile fst (map getCell [1..]) of
    [] -> error "should never happen"
    (x:_) -> snd x
  where
    getCell i =
      let newX = x + (dx * i) in
      let newY = y + (dy * i) in
      -- trace (show i ++ "*" ++ show (dx, dy) ++ " -> " ++ show (x, y) ++ ": " ++ show (newX, newY)) newY
      if newY < 0 || newY >= length grid || newX < 0 || newX >= length (grid!!newY) then (False, '.')
      else (if grid!!newY!!newX == '.' then (True, '.') else (False, grid!!newY!!newX))

getLineOfSightNeighbors :: [String] -> Int -> Int -> String
getLineOfSightNeighbors grid currX currY =
  let directions = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], not (dx == 0 && dy == 0)] in
  map (uncurry (findFirstSeat grid currX currY)) directions

getNeighbors :: [String] -> Int -> Int -> String
getNeighbors grid currX currY =
  let positions = [(currX + dx, currY + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]] in
  let isNeighbor = \(x, y) -> y >= 0 && y < length grid && x >= 0 && x < length (grid!!y) && not (x == currX && y == currY) in
  [grid!!y!!x | (x, y) <- filter isNeighbor positions]

{-
  If a seat is empty (L) and there are no occupied seats adjacent to it,
  the seat becomes occupied.
-}
shouldFlipToOccupied :: [String] -> Int -> Int -> Bool
shouldFlipToOccupied grid x y
  | grid!!y!!x == 'L' =
    '#' `notElem` getNeighbors grid x y
  | otherwise = False

{-
  If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
-}
shouldFlipToEmpty :: [String] -> Int -> Int -> Bool
shouldFlipToEmpty grid x y
  | grid!!y!!x == '#' =
    let adjacentOccupied = filter (== '#') (getNeighbors grid x y) in
    length adjacentOccupied >= 4
  | otherwise = False

{-
  If a seat is empty (L) and there are no occupied seats adjacent to it,
  the seat becomes occupied.
-}
shouldFlipToOccupiedPart2 :: [String] -> Int -> Int -> Bool
shouldFlipToOccupiedPart2 grid x y
  | grid!!y!!x == 'L' =
    '#' `notElem` getLineOfSightNeighbors grid x y
  | otherwise = False

{-
  If a seat is occupied (#) and five or more seats adjacent to it are also occupied, the seat becomes empty.
-}
shouldFlipToEmptyPart2 :: [String] -> Int -> Int -> Bool
shouldFlipToEmptyPart2 grid x y
  | grid!!y!!x == '#' =
    let adjacentOccupied = filter (== '#') (getLineOfSightNeighbors grid x y) in
    length adjacentOccupied >= 5
  | otherwise = False


type NextGenerationFactory = [String] -> Int -> Int -> Char

getNextGenerationValuePart1 :: NextGenerationFactory
getNextGenerationValuePart1 grid x y
  | shouldFlipToOccupied grid x y = '#'
  | shouldFlipToEmpty grid x y = 'L'
  | otherwise = grid!!y!!x

getNextGenerationValuePart2 :: NextGenerationFactory
getNextGenerationValuePart2 grid x y
  | shouldFlipToOccupiedPart2 grid x y = '#'
  | shouldFlipToEmptyPart2 grid x y = 'L'
  | otherwise = grid!!y!!x

findStableGeneration :: NextGenerationFactory -> [String] -> [String]
findStableGeneration fNextGen grid =
  let nextGrid = [[fNextGen grid x y | x <- [0..(length (head grid) - 1)]] | y <- [0..(length grid - 1)]] in
  if showGrid grid == nextGrid then grid else findStableGeneration fNextGen nextGrid

countOccupied :: [String] -> Int
countOccupied grid =
  length (concatMap (filter (== '#')) grid)

showGrid :: [String] -> [String]
showGrid grid =
  trace (intercalate "\n" grid ++ "\n\n") grid

day11 :: [String] -> String
day11 lines = do
  -- show (shouldFlipToEmptyPart2 lines 0 1)
  show (countOccupied (showGrid (findStableGeneration getNextGenerationValuePart2 lines)))