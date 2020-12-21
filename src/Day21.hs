module Day21 where

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Text.Regex.PCRE
import Data.List
import Data.Ord
import Data.Bits

-- mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
parseIngredients :: String -> [(String, Set.Set String)]
parseIngredients line =
  let (_, _, _, matches) = line =~ "(.*) \\(contains (.*)\\)" :: (String, String, String, [String]) in
  let ingredients = splitOn " " (head matches) in
  let allergens = splitOn ", " (matches!!1) in
  [(a, Set.fromList ingredients) | a <- allergens]

hasAllergens :: String -> Bool
hasAllergens line =
  line =~ "contains.*" :: Bool

getAllIngredients :: String -> [String]
getAllIngredients line =
  let chunks = splitOn " " line in
  takeWhile (not . ('(' `elem`)) chunks

countOccurrences :: [String] -> String -> Int
countOccurrences list ingredient  =
  length (filter (== ingredient) list)

without :: Map.Map String (Set.Set String) -> String -> String -> Map.Map String (Set.Set String)
without candidateMap allergen selectedCandidate =
  Map.delete allergen (Map.map (Set.filter (/= selectedCandidate)) candidateMap)

solveIngredients :: Map.Map String String -> Map.Map String (Set.Set String) -> (Bool, Map.Map String String)
solveIngredients solved toSolve
  | null toSolve = (True, solved)
  | or [null candidates | candidates <- Map.elems toSolve] = (False, Map.empty)
  | otherwise =
     let (allergen, candidates) = minimumBy (comparing (length . snd)) (Map.assocs toSolve) in
     let possibleSolutions = [solveIngredients (Map.insert allergen c solved) (without toSolve allergen c) | c <- Set.elems candidates] in
     let validSolutions = filter fst possibleSolutions in
     if null validSolutions then (False, Map.empty) else head validSolutions

solve :: [String] -> String
solve lines = do
  let allIngredients = concatMap getAllIngredients lines
  let allergenLines = filter hasAllergens lines
  let allergenMap = Map.fromListWith Set.intersection (concatMap parseIngredients allergenLines)
  let (isSolved, solution) = solveIngredients Map.empty allergenMap
  if isSolved then
    show $ intercalate "," (map snd (sortBy (comparing fst) (Map.assocs solution)))
  else
    error "no solution"
  {-
    (part 1)
    let possibleAllergens = foldl Set.union Set.empty (Map.elems allergenMap)
    let impossibleOnes = Set.elems (Set.difference (Set.fromList allIngredients) possibleAllergens)
    show $ sum (map (countOccurrences allIngredients) impossibleOnes)
  -}
