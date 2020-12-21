module Day20 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

data Tile = Tile
    { tileNumber     :: Int
    , top            :: String
    , right          :: String
    , bottom         :: String
    , left           :: String
    , grid           :: [String]
    } deriving (Eq, Ord, Show)

tileSize :: Tile -> Int
tileSize tile = length (top tile)

ofGrid :: Int -> [String] -> Tile
ofGrid tileNumber tileGrid =
  let topBoundary = head tileGrid in
  let bottomBoundary = last tileGrid in
  let leftBoundary = [head l | l <- tileGrid] in
  let rightBoundary = [last l | l <- tileGrid] in
  Tile tileNumber topBoundary rightBoundary bottomBoundary leftBoundary tileGrid

withNewGrid :: Tile -> [String] -> Tile
withNewGrid tile = ofGrid (tileNumber tile)

{-
  withNewBorders :: Tile -> [String] -> Tile
  withNewBorders tile border =
  Tile (tileNumber tile) (head border) (border!!1) (border!!2) (border!!3)
-}

borders :: Tile -> [String]
borders tile =
  [top tile, right tile, bottom tile, left tile]

rotateClockwise :: Tile -> Tile
rotateClockwise tile =
  let tileGrid = grid tile in
  let lim = tileSize tile - 1 in
  withNewGrid tile [[tileGrid!!(lim - y)!!x | y <- [0..lim]] | x <- [0..lim]]

rotationsRec :: [Tile] -> [Tile]
rotationsRec rotationsSoFar
  | length rotationsSoFar == 4 = rotationsSoFar
  | otherwise =
    let tile = last rotationsSoFar in
    rotationsRec (rotationsSoFar ++ [rotateClockwise tile])

rotations tile =
  rotationsRec [tile]

horizontalFlip :: Tile -> Tile
horizontalFlip tile =
  let tileGrid = grid tile in
  withNewGrid tile (reverse tileGrid)

verticalFlip :: Tile -> Tile
verticalFlip tile =
  let tileGrid = grid tile in
  withNewGrid tile (map reverse tileGrid)

reflections :: Tile -> [Tile]
reflections tile =
  [tile, verticalFlip tile, horizontalFlip tile, verticalFlip . horizontalFlip $ tile]

tilePermutations :: Tile -> [Tile]
tilePermutations tile =
  let allPermutations = concatMap rotations (reflections tile) in
  Set.elems (Set.fromList allPermutations)

parseTile :: [String] -> Tile
parseTile lines =
  let (_, _, _, matches) = head lines =~ "Tile ([0-9]*):" :: (String, String, String, [String]) in
  let tileId = read (head matches) :: Int in
  ofGrid tileId (tail lines)

isValid :: Map.Map (Int, Int) Tile -> (Int, Int) -> Tile -> Bool
isValid placements (x, y) tile =
  let tp = case Map.lookup (x, y -1) placements of
             Nothing -> True
             Just other -> top tile == bottom other in
  let bp = case Map.lookup (x, y + 1) placements of
             Nothing -> True
             Just other -> bottom tile == top other in
  let lp = case Map.lookup (x - 1, y) placements of
             Nothing -> True
             Just other -> left tile == right other in
  let rp = case Map.lookup (x + 1, y) placements of
             Nothing -> True
             Just other -> right tile == left other in
  and [tp, bp, lp, rp]


without :: Tile -> [Int] -> [Int]
without tile tiles =
  [t | t <- tiles, t /= tileNumber tile]


allEligiblePermutations :: [Int] -> Map.Map Int [Tile] -> [Tile]
allEligiblePermutations tileNumbers permutationMap =
  concat [just (Map.lookup t permutationMap) | t <- tileNumbers]

findValidPlacement :: [Int] -> [(Int, Int)] -> Map.Map Int [Tile] -> Map.Map (Int, Int) Tile -> (Bool, Map.Map (Int, Int) Tile)
findValidPlacement remainingTiles remainingSlots permutationMap existingPlacements
  | null remainingTiles || null remainingSlots = (True, existingPlacements)
  | otherwise =
      let toPlace = head remainingSlots in
      let predicate = isValid existingPlacements toPlace in
      let candidates = filter predicate (allEligiblePermutations remainingTiles permutationMap) in
      let results = map (\c -> findValidPlacement (without c remainingTiles) (tail remainingSlots) permutationMap (Map.insert toPlace c existingPlacements)) candidates in
      let successfulPlacements = filter fst results in
      if null successfulPlacements then (False, existingPlacements) else head successfulPlacements


displayTileSize :: Tile -> Int
displayTileSize tile =
  tileSize tile - 2

displayGrid :: Tile -> [String]
displayGrid tile =
  let num = displayTileSize tile in
  [take num . drop 1 $ row | row <- take num . drop 1 $ grid tile]

showTileRow :: Tile -> Int -> String
showTileRow tile row =
  displayGrid tile!!row

showTilesRow :: [Tile] -> [String]
showTilesRow tiles =
  [concatMap (`showTileRow` row) tiles | row <- [0..(displayTileSize (head tiles) - 1)]]

just maybe =
  case maybe of
    Just x -> x
    Nothing -> error "expected value"

showPlacement :: Map.Map (Int, Int) Tile -> Int -> [String]
showPlacement placement squareDimension =
  let grid = [[just (Map.lookup (i,j) placement) | i <- [0..squareDimension - 1]] | j <- [0..squareDimension - 1]] in
  concatMap showTilesRow grid

showRowIds :: [Tile] -> String
showRowIds row =
  intercalate " " [show (tileNumber t) | t <- row]

showIds placement squareDimension =
  let grid = [[just (Map.lookup (i,j) placement) | i <- [0..squareDimension - 1]] | j <- [0..squareDimension - 1]] in
  intercalate "\n" (map showRowIds grid)

getAnswer placement squareDimension =
  let i = squareDimension - 1 in
  let tNumber = \(x, y) -> tileNumber (just (Map.lookup (x, y) placement)) in
  tNumber (0, 0) * tNumber (0, i) * tNumber (i, i) * tNumber (i, 0)

{-
                    #
  #    ##    ##    ###
   #  #  #  #  #  #

  check if sea-monster at this index, where index is the tail of the sea-monster
 -}

isCheckmark :: [String] -> (Int, Int) -> Bool
isCheckmark grid (x,y)
  | y < 0 || x < 0 || y >= length grid || x >= length (grid!!y) = False
  | otherwise = grid!!y!!x == '#'

monsterOffsets = [ (18, -1), (0, 0), (5, 0), (6, 0), (11, 0), (12, 0)
                  ,(17, 0), (18, 0), (19, 0), (1, 1), (4, 1), (7, 1), (10, 1)
                  ,(13, 1), (16, 1) ]
isSeaMonsterAtIndex :: [String] -> (Int, Int) -> Bool
isSeaMonsterAtIndex grid (x, y) =
    and [isCheckmark grid (x + dx, y + dy) | (dx, dy) <- monsterOffsets]

getSeaMonsters :: [String] -> [(Int, Int)]
getSeaMonsters grid =
  [(x, y) | y <- [0..length grid -1], x <- [0..length (head grid) - 1], isSeaMonsterAtIndex grid (x, y)]

solve :: [String] -> String
solve lines = do
  let tiles = map parseTile (splitOn [""] lines)
  let tileNumbers = [tileNumber t | t <- tiles]
  let tile1 = head tiles
  let squareDimension = round . sqrt . fromIntegral $ length tiles
  let slotsToFill = concat [[(i,j) | i <- [0..squareDimension - 1]] | j <- [0..squareDimension - 1]]
  let permutations = Map.fromList [(tileNumber tile, tilePermutations tile) | tile <- tiles]
  let (foundPlacements, placement) =  findValidPlacement tileNumbers slotsToFill permutations Map.empty
  let rendered = showPlacement placement squareDimension
  -- show $ isSeaMonsterAtIndex (trace (intercalate "\n" rendered) rendered) (2, 3)
  let totalCheckmarks = length $ filter (== '#') (concat rendered)
  let totalMonsterCheckmarks = length monsterOffsets * length (getSeaMonsters rendered)
  show (totalCheckmarks - (trace ("totalMonstercheckmarks: " ++ show totalMonsterCheckmarks) totalMonsterCheckmarks))