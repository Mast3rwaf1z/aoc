module Main where

import Text.Regex.Posix ((=~), MatchArray)
import Text.RawString.QQ (r)
import Data.List (transpose)


testData :: String
testData = [r|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|]

pattern :: String
pattern = "XMAS"

getDiagonals :: [[a]] -> [[a]]
getDiagonals matrix = upper ++ lower
  where
    size = length matrix
    -- Get upper diagonals (including the main diagonal)
    upper = [ [matrix !! i !! (k - i) | i <- [0..k], i < size, (k - i) < size] | k <- [0..size-1] ]
    -- Get lower diagonals (below the main diagonal)
    lower = [ [matrix !! i !! (k - i) | i <- [(k - size + 1)..(size-1)], i < size, (k - i) < size] | k <- [size..2*size-2] ]

-- Pad each diagonal with spaces to form a diamond
padDiagonals :: [[Char]] -> [[Char]]
padDiagonals diags = map padLine (zip [0..] diags)
  where
    maxWidth = length (last diags)
    padLine (i, line) = replicate (maxWidth - i - 1) ' ' ++ line

-- Convert each row to a string for display

-- Rotate a matrix 45 degrees as a diamond pattern
rotate45 :: [[Char]] -> [String]
rotate45 matrix =
  let diags = getDiagonals matrix
      padded = padDiagonals diags
  in padded

rotate90 :: [String] -> [String]
rotate90 = reverse . transpose

matrices :: [String] -> [[String]]
matrices matrix = 
    [ rotate45 matrix
    , rotate90 matrix
    , rotate45 . rotate90 $ matrix
    , rotate90 . rotate90 $ matrix
    , rotate45 . rotate90 . rotate90 $ matrix
    , rotate90 . rotate90 . rotate90 $ matrix
    , rotate45 . rotate90 . rotate90 . rotate90 $ matrix
    , matrix
    ]

matchPattern :: String -> Int
matchPattern input = sum $ map (\x -> length (unlines x =~ pattern :: [MatchArray])) $ matrices . lines $ input

main :: IO ()
main = do
    input <- readFile "./inputs/day4_input.txt"
    putStrLn "==> Day 4"
    putStrLn "Test Data"
    print $ matchPattern testData
    putStrLn "exercise data"
    print $ matchPattern input
