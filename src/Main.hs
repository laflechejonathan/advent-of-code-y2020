module Main where

import Day21

main :: IO ()
main = do
  content <- readFile "./data/day21.txt"
  let result = Day21.solve (lines content)
  putStr result