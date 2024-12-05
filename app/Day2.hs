module Main where
import Data.List.Split (splitOn)
import Data.List (sort)

-- task 1

largerIsSafe :: [Int] -> Bool
largerIsSafe [] = True
largerIsSafe [_] = True
largerIsSafe [x, y] = x < y && abs y-x <= 3
largerIsSafe (x:y:xs) = x < y && abs y-x <= 3 && largerIsSafe (y:xs)

smallerIsSafe :: [Int] -> Bool
smallerIsSafe [] = True
smallerIsSafe [_] = True
smallerIsSafe [x, y] = x > y && abs x-y <= 3
smallerIsSafe (x:y:xs) = x > y && abs x-y <= 3 && smallerIsSafe (y:xs)

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [_] = True
isSafe (x:y:xs) | x < y && abs y-x <= 3 = largerIsSafe (y:xs)
                | x > y && abs x-y <= 3 = smallerIsSafe (y:xs)
                | otherwise = False

testData :: [[Int]]
testData = [
    [7, 6, 4, 2, 1],
    [1, 2, 7, 8, 9],
    [9, 7, 6, 2, 1],
    [1, 3, 2, 4, 5],
    [8, 6, 4, 4, 1],
    [1, 3, 6, 7, 9]
    ]

-- task 2
remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_:xs) = xs
remove n (x:xs) = x : remove (n-1) xs

isSafeDrop1 :: Int -> [Int] -> Bool
isSafeDrop1 index xs    | index == length xs = False
                        | isSafe $ remove index xs = True
                        | otherwise = isSafeDrop1 (index+1) xs

main :: IO ()
main = do
    putStrLn "==> Task 1"
    putStrLn "Test data"
    print $ length $ filter isSafe testData
    exerciseData <- map (map (\x -> read x :: Int)) . (map $ splitOn " ") . lines <$> readFile "inputs/day2_input.txt"
    let filteredData = filter isSafe exerciseData
    print $ length filteredData
    putStrLn "==> Task 2"
    putStrLn "Test Data"
    print $ length $ filter (isSafeDrop1 0) testData
    print $ length $ filter (isSafeDrop1 0) exerciseData
