module Main where

import Data.List (sort)
import Data.Bifunctor (Bifunctor(bimap))

-- Task 1

pairData :: [(Int, Int)] -> [(Int, Int)]
pairData xs = zip first second
    where
        (first, second) = bimap sort sort $ unzip xs

getDistances :: [(Int, Int)] -> [Int]
getDistances = map $ abs . uncurry (-)

totalDistance :: [Int] -> Int
totalDistance = sum

-- Task 2
similarity :: [(Int, Int)] -> Int
similarity = _similarity . unzip
    where
        _similarity :: ([Int], [Int]) -> Int
        _similarity (xs, ys) = sum $ map (\x -> x * (length . filter (x==)) ys) xs 

testData :: [(Int, Int)]
testData = [(4, 2), (3, 3), (2, 4), (1, 5)]

parseData :: [String] -> [(Int, Int)]
parseData [] = []
parseData (x:xs) = ((read $ take 5 x) :: Int, (read $ drop 8 x) :: Int) : parseData xs

main :: IO ()
main = do
    putStrLn "==> Task 1"
    putStrLn "Test data"
    print $ pairData testData
    print $ (getDistances . pairData) testData
    print $ (totalDistance . getDistances . pairData) testData
    exerciseData <- parseData . lines <$> readFile "inputs/day1_input.txt"
    print $ (totalDistance . getDistances . pairData) exerciseData

    putStrLn "==> Task 2"
    putStrLn "Test data"
    print $ similarity testData
    print $ similarity exerciseData

