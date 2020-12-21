module Day2 where

import Text.Regex.PCRE


-- e.g. 9-11 p: pppppppppxblp
data PasswordEntry = PasswordEntry { firstNumber :: Int
                     , secondNumber :: Int
                     , letter :: Char
                     , password ::String
                     } deriving ( Show )

parseLine :: String -> PasswordEntry
parseLine line =
  if length matches > 0 then
    PasswordEntry { firstNumber=(read (matches!!0) :: Int), secondNumber=(read (matches!!1) :: Int), letter=(matches!!2!!0), password=(matches!!3)}
  else
    error ("No match for: " ++ line)
  where (_, _, _, matches) = line =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]*)" :: (String, String, String, [String])

isPart1Valid :: PasswordEntry -> Bool
isPart1Valid entry =
     letterCount >= (firstNumber entry) && letterCount <= (secondNumber entry)
     where
       letterCount = length (filter (\c -> c == (letter entry)) (password entry))


containsAtIndex :: String -> Char -> Int -> Bool
containsAtIndex string letter index =
  -- 1 indexing
  (index - 1) < (length string) && string!!(index - 1) == letter

isPart2Valid :: PasswordEntry -> Bool
isPart2Valid entry =
     (containsAtIndex pwd ltr (firstNumber entry)) /= (containsAtIndex pwd ltr (secondNumber entry))
     where
       pwd = password entry
       ltr = letter entry

day2 :: [String] -> String
day2 lines = do
  let entries = map parseLine lines
  show (length (filter isPart2Valid entries))

