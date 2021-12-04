readDepths :: [String] -> [Integer]
readDepths = map read

countIncreasingDepth :: [Integer] -> Integer
countIncreasingDepth (x:y:xs) = (if x < y then 1 else 0) + countIncreasingDepth (y : xs)
countIncreasingDepth xs = 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ countIncreasingDepth $ readDepths $ lines input
