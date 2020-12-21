module Day3 where

-- data Tree = Tree deriving (Show)
-- data OpenSpace = OpenSpace deriving (Show)
data Point = Tree | OpenSpace deriving (Show)
data Coordinate = Coordinate Int Int deriving (Show)

parsePoint :: Char -> Point
parsePoint char =
  case char of
  '#' -> Tree
  '.' -> OpenSpace
  c -> error ("Unexpected character: " ++ [c])

pointAtIndex :: [Point] -> Int -> Point
pointAtIndex points index =
  points!!(index `mod` (length points))

countTobogganTrees :: Coordinate -> Coordinate -> [[Point]] -> Int
countTobogganTrees (Coordinate x y) (Coordinate dx dy) points =
  if y >= (length points) then
  0
  else
  (countTobogganTrees (Coordinate (x + dx) (y + dy)) (Coordinate dx dy) points) +
    (case (pointAtIndex (points!!y) x) of
      Tree -> 1
      OpenSpace -> 0)

day3 :: [String] -> String
day3 lines = do
  --- [ [Point] ]
  let points = map (\l -> (map parsePoint l)) lines
  let slopes = [Coordinate 1 1
               ,Coordinate 3 1
               ,Coordinate 5 1
               ,Coordinate 7 1
               ,Coordinate 1 2]
  let treeCounts = (map (\c -> countTobogganTrees (Coordinate 0 0) c points) slopes)
  show (foldl (*) 1 treeCounts)
