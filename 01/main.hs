-- https://adventofcode.com/2021/day/1

readDepths :: [String] -> [Integer]
readDepths = map read

countIncreasingDepth :: [Integer] -> Integer
countIncreasingDepth (x:y:xs) = (if x < y then 1 else 0) + countIncreasingDepth (y : xs)
countIncreasingDepth _ = 0

getMeasurementWindows :: [Integer] -> [Integer]
getMeasurementWindows (x:y:z:xs) = x+y+z : getMeasurementWindows (y:z:xs)
getMeasurementWindows _ = []

-- First assignment
ptOne :: IO ()
ptOne = do
  input <- readFile "input.txt"
  print $ countIncreasingDepth $ readDepths $ lines input

-- Second assignment
ptTwo :: IO ()
ptTwo = do
  input <- readFile "input.txt"
  print $ countIncreasingDepth $ getMeasurementWindows $ readDepths $ lines input