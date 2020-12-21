module Day12 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List

{-
  Action N means to move north by the given value.
  Action S means to move south by the given value.
  Action E means to move east by the given value.
  Action W means to move west by the given value.
  Action L means to turn left the given number of degrees.
  Action R means to turn right the given number of degrees.
  Action F means to move forward by the given value in the direction the ship is currently facing.

  e.g.
  F10
  N3
  F7
  R90
  F11
-}

headingIndex :: Char -> Int
headingIndex heading =
  case elemIndex heading "SWNE" of
    Just n -> n
    Nothing -> error ("unknown heading: " ++ show heading)


applyHeading :: Char -> Int -> Char
applyHeading heading delta =
  let moves = (delta `div` 90) `mod` 4 in
  let headings = "SWNE" in
  let newIndex = headingIndex heading + moves in
  case newIndex of
    i | i < 0               -> headings!!(length headings - i)
      | i >= length headings -> headings!!(i - length headings)
      | otherwise           -> headings!!i

navigate :: (Char, Int, Int) -> String -> (Char, Int, Int)
navigate (heading, x, y) instruction =
  let actionType = head instruction in
  let number = read (tail instruction) :: Int in
  case actionType of
    'N' -> (heading, x, y + number)
    'S' -> (heading, x, y - number)
    'E' -> (heading, x + number, y)
    'W' -> (heading, x - number, y)
    'L' -> (applyHeading heading (0 - number), x, y)
    'R' -> (applyHeading heading number, x, y)
    'F' -> navigate (heading, x, y) (heading : (show number))
    x -> error ("unknown action: " ++ show x)


{-
   10   11   12   13
  [W]  [.]  [.]  [S]
  F2
  dx = -3
  newShipX = 13 + (2*-3) = 7
  newWaypointDx = newShipX + dx = 7 - 3 = 4
-}
forward :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
forward (shipX, shipY, dx, dy) amount =
  let newShipX = shipX + (dx * amount) in
  let newShipY = shipY + (dy * amount) in
  (newShipX, newShipY, dx, dy)

{-
   . . . . . . .
   . . W . . . .
   . . . . . x .
   . . . S . . .
   . z . . . . .
   . . . . y . .

   ship = 3,3
   waypoint vector = (-1, 2)
   90d right = (2, 1)
   180d right = (1, -2)
   270d right = (-2, -1)
-}
rotateWaypointClockwise :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
rotateWaypointClockwise (shipX, shipY, waypointDx, waypointDy) amount =
  let rotations = (amount `div` 90 `mod` 4) in
  case rotations of
    0 -> (shipX, shipY, waypointDx, waypointDy) -- identity
    1 -> (shipX, shipY, waypointDy, negate waypointDx) -- one clockwise turn
    2 -> (shipX, shipY, negate waypointDx, negate waypointDy) -- two clockwise turns
    3 -> (shipX, shipY, negate waypointDy, waypointDx) -- three clockwise turns
    _ -> error "unsupported rotation"


navigate2 :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
navigate2 (shipX, shipY, waypointDx, waypointDy) instruction =
  let actionType = head instruction in
  let number = read (tail instruction) :: Int in
  case (trace (show (shipX, shipY, waypointDx, waypointDy, instruction)) actionType) of
    'N' -> (shipX, shipY, waypointDx, waypointDy + number)
    'S' -> (shipX, shipY, waypointDx, waypointDy - number)
    'E' -> (shipX, shipY, waypointDx + number, waypointDy)
    'W' -> (shipX, shipY, waypointDx - number, waypointDy)
    'L' -> rotateWaypointClockwise (shipX, shipY, waypointDx, waypointDy) (360 - number)
    'R' -> rotateWaypointClockwise (shipX, shipY, waypointDx, waypointDy) number
    'F' -> forward (shipX, shipY, waypointDx, waypointDy) number
    x -> error ("unknown action: " ++ show x)

getEndPosition :: [String] -> (Char, Int, Int) -> (Char, Int, Int)
getEndPosition instructions (heading, x, y) =
  case instructions of
    [] -> (heading, x, y)
    instruction:others -> getEndPosition others (navigate (heading, x, y) instruction)

getEndPosition2 :: [String] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
getEndPosition2 instructions (shipX, shipY, waypointDx, waypointDy) =
  case instructions of
    [] -> (shipX, shipY, waypointDx, waypointDy)
    instruction:others -> getEndPosition2 others (navigate2 (shipX, shipY, waypointDx, waypointDy) instruction)

day12 :: [String] -> String
day12 lines = do
  -- part1
  -- let (lastHeading, x, y) = getEndPosition lines ('E', 0, 0)
  let (x, y, _, _) = getEndPosition2 lines (0, 0, 10, 1)
  show (abs x + abs (trace (show x ++ "," ++ show y) y))
