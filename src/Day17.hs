module Day17 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

type PocketDimension = Map.Map (Int, Int, Int, Int) Bool
type DimensionBounds = ((Int, Int), (Int, Int), (Int, Int), (Int, Int))

get :: PocketDimension -> (Int, Int, Int, Int) -> Bool
get pocketDimension key =
  Just True == Map.lookup key pocketDimension

expand :: DimensionBounds -> DimensionBounds
expand ((minW, maxW), (minZ, maxZ), (minY, maxY), (minX, maxX)) =
  ((minW - 1, maxW + 1), (minZ - 1, maxZ + 1), (minY - 1, maxY + 1), (minX - 1, maxX + 1))

applyInBounds :: ((Int, Int, Int, Int) -> Bool) -> DimensionBounds -> PocketDimension
applyInBounds predicate ((minW, maxW), (minZ, maxZ), (minY, maxY), (minX, maxX)) =
  Map.fromList [((w, z, y, x), predicate (w, z, y, x)) | w <- [minW..maxW], z <- [minZ..maxZ], y <- [minY..maxY], x <- [minX..maxX]]

{-
  initial state:
  .#.
  ..#
  ###

  '#' = active
  '.' inactive


  If a cube is active:
    If exactly 2 or 3 of its neighbors are also active: the cube remains active.
    Otherwise: the cube becomes inactive.
  If a cube is inactive:
    If exactly 3 of its neighbors are active: the cube becomes active.
    Otherwise: the cube remains inactive.
-}
applyInBoundsRules :: PocketDimension -> (Int, Int, Int, Int) -> Bool
applyInBoundsRules pocketDimension (w, z, y, x) =
  let neighbors = [get pocketDimension (w + nw, z + nz, y + ny, x + nx) | nx <- [-1..1], ny <- [-1..1], nz <- [-1..1], nw <- [-1..1], not (nx == 0 && ny == 0 && nz == 0 && nw == 0)] in
  let activeNeighborCount = length (filter id neighbors) in
  (if get pocketDimension (w, z, y, x) then
       activeNeighborCount == 2 || activeNeighborCount == 3
   else
       activeNeighborCount == 3)

transform :: PocketDimension -> DimensionBounds -> Int -> (PocketDimension, DimensionBounds)
transform pocketDimension bounds targetGeneration
  | targetGeneration == 0 = (pocketDimension, bounds)
  | otherwise =
    let nextGen = applyInBounds (applyInBoundsRules pocketDimension) bounds in
    transform nextGen (expand bounds) (targetGeneration  - 1)

{-
show2dLayer :: Int -> PocketDimension -> (Int, Int) -> (Int, Int) -> String
show2dLayer zLayer pocketDimension (minY, maxY) (minX, maxX) =
  let layerLabel = "z=" ++ show zLayer in
  let stringGrid = intercalate "\n" [[if get pocketDimension (zLayer, y, x) then '#' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]] in
  layerLabel ++ "\n" ++ stringGrid

showPocketDimension :: PocketDimension -> DimensionBounds -> String
showPocketDimension pocketDimension ((minZ, maxZ), (minY, maxY), (minX, maxX)) =
  intercalate "\n\n" [show2dLayer z pocketDimension (minY, maxY) (minX, maxX) | z <- [minZ..maxZ]]
-}

isActiveInInput :: [String] -> (Int, Int, Int, Int) -> Bool
isActiveInInput lines (w, z, y, x) =
  w == 0 && z == 0 && y >= 0 && y < length lines && x >= 0 && x < length (lines!!y) && lines!!y!!x == '#'


countActive :: PocketDimension -> Int
countActive pocketDimension =
  length (filter id (Map.elems pocketDimension))

solve :: [String] -> String
solve lines = do
  let pocketDimension = Map.fromList [((0, 0, y, x), isActiveInInput lines (0, 0, y, x)) | x <- [0..(length (head lines))], y <- [0..(length lines)]]
  let bounds = ((-1, 1), (-1, 1), (-1, length lines + 1), (-1, length (head lines) + 1))
  let (newDimension, newBounds) = transform pocketDimension bounds 6
  show (countActive newDimension)
