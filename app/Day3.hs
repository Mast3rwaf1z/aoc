module Main where

import Text.RawString.QQ (r)
import Text.Regex (matchRegex, mkRegex, subRegex)

-- Task 1
regex :: String
regex = [r|mul\(([0-9]+),([0-9]+)\)|]

remove :: String -> String -> String
remove _ [] = []
remove target (x:xs)    | target == take (length target) (x:xs) = drop (length target) (x:xs)
                        | otherwise = remove target xs

task1 :: String -> Int
task1 xs = case matchRegex (mkRegex regex) xs of
    Just [first, second] -> (read first :: Int) * (read second :: Int) + (task1 $ remove ("mul("++first++","++second++")") xs)
    _ -> 0

-- Task 2
-- cursed chatgpt result because the regex library doesn't support a lot of entry level regex functionality...
removeBetween :: String -> String
removeBetween "" = ""
removeBetween str =
  case splitAtPattern "don't()" str of
    Nothing -> str -- No more patterns, return the remaining string
    Just (before, rest) ->
      case splitAtPattern "do()" rest of
        Nothing -> before -- No matching 'do()', keep the prefix
        Just (_, after) -> before ++ removeBetween after

splitAtPattern :: String -> String -> Maybe (String, String)
splitAtPattern pattern str =
  case breakOn pattern str of
    Nothing -> Nothing
    Just (before, rest) -> Just (before, drop (length pattern) rest)

breakOn :: String -> String -> Maybe (String, String)
breakOn pattern str = 
  case splitAtIndex (findIndex pattern str) str of
    Nothing -> Nothing
    Just (before, after) -> Just (before, after)

findIndex :: String -> String -> Maybe Int
findIndex pattern str =
  let len = length pattern
  in case filter (\i -> take len (drop i str) == pattern) [0..length str - len] of
       [] -> Nothing
       (i:_) -> Just i

splitAtIndex :: Maybe Int -> String -> Maybe (String, String)
splitAtIndex Nothing _ = Nothing
splitAtIndex (Just idx) str = Just (take idx str, drop idx str)

testData :: String
testData = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

main :: IO ()
main = do
    putStrLn "==> Day 3"
    print $ task1 testData
    exerciseData <- filter (\x -> x /= '\n') <$> readFile "./inputs/day3_input.txt"
    print $ task1 exerciseData
    print $ task1 $ removeBetween exerciseData
