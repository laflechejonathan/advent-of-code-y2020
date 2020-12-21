
module Day4 where

import Debug.Trace
import Data.List.Split
import Text.Regex.PCRE
import qualified Data.Map as Map

splitKeyValue :: String -> (String, String)
splitKeyValue string =
  case splitOn ":" string of
    [k, v] -> (k, v)
    _ -> error ("no valid split: " ++ string)

batchPassports :: [String] -> [String] -> [[String]]
batchPassports lines batchInProgress =
  case lines of
    [] -> [batchInProgress]
    line:otherLines -> if (length line) == 0
        then [batchInProgress] ++ (batchPassports otherLines [])
        else (batchPassports otherLines (batchInProgress ++ [line]))

parsePassport :: [String] -> Map.Map String String
parsePassport lines =
  Map.fromList((concat (map (\l-> (map splitKeyValue (splitOn " " l))) lines)))

{-
  byr (Birth Year)
  iyr (Issue Year)
  eyr (Expiration Year)
  hgt (Height)
  hcl (Hair Color)
  ecl (Eye Color)
  pid (Passport ID)
  cid (Country ID)  # optional

  byr (Birth Year) - four digits; at least 1920 and at most 2002.
  iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  hgt (Height) - a number followed by either cm or in:
  If cm, the number must be at least 150 and at most 193.
  If in, the number must be at least 59 and at most 76.
  hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  pid (Passport ID) - a nine-digit number, including leading zeroes.
-}
valueInRange :: Int -> Int -> String -> Bool
valueInRange min max str =
  (str =~ "^[0-9]+$")
    && (let num = (read str :: Int) in num >= min && num <= max)

validBirthYear = valueInRange 1920 2002
validIssueYear = valueInRange 2010 2020
validExpirationYear = valueInRange 2020 2030

validHeight :: String -> Bool
validHeight str =
  ((length matches) > 0)
    &&
      (case matches !! 1 of
         "cm" -> valueInRange 150 193 (head matches)
         "in" -> valueInRange 59 76 (head matches)
         _ -> error ("Unexpected unit:" ++ str))
  where (_, _, _, matches) = str =~ "([0-9]+)(in|cm)" :: (String, String, String, [String])

validHairColor :: String -> Bool
validHairColor str =
  str =~ "^#[0-9a-f]{6}$" :: Bool

validEyeColor :: String -> Bool
validEyeColor str =
  str =~ "amb|blu|brn|gry|grn|hzl|oth" :: Bool

validPassportNumber :: String -> Bool
validPassportNumber str =
  str =~ "^[0-9]{9}$" :: Bool

isValidPassport :: Map.Map String String -> Bool
isValidPassport fields =
  let validations = map (\(key, predicate) -> (case Map.lookup key fields of
       Just value -> (key, predicate value)
       Nothing -> (key, False)))
         [ ("byr", validBirthYear)
         , ("iyr", validIssueYear)
         , ("eyr", validExpirationYear)
         , ("hgt", validHeight)
         , ("hcl", validHairColor)
         , ("ecl", validEyeColor)
         , ("pid", validPassportNumber)] in
  all (\(_, isTrue) -> isTrue) (trace (show validations) validations)


day4 :: [String] -> String
day4 lines = do
  let batches = batchPassports lines []
  let passports = map parsePassport batches
  show (length (filter isValidPassport passports))

