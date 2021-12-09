import Data.Char (digitToInt)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

linesToHeightmap :: [String] -> [[Int]]
linesToHeightmap = map (map digitToInt)

isOutOfBounds :: [[Int]] -> (Int, Int) -> Bool
isOutOfBounds matrix (x, y) = x < 0 || y < 0 || x >= maxX || y >= maxY
  where
    maxX = length matrix
    maxY = length (head matrix)

findSurrounding :: [[Int]] -> (Int, Int) -> [Int]
findSurrounding matrix (x, y) = map (getElementAt matrix) positions
  where
    positions = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getElementAt :: [[Int]] -> (Int, Int) -> Int
getElementAt matrix point@(x, y) = if isOutOfBounds matrix point then -1 else (matrix !! y) !! x

isPointLowest :: Int -> [Int] -> Bool
isPointLowest x = all (> x)

getLowPoints :: [[Int]] -> [Int]
getLowPoints matrix = foldl (\acc (x, row) -> acc ++ getLowPointsFromRow x row) [] (enumerate matrix)
  where
    getLowPointsFromRow xa row = map snd (filter (\(ya, point) -> isPointLowest point (findSurrounding matrix (xa, ya))) (enumerate row))

sumLowPoints :: [Int] -> Int
sumLowPoints = foldl (\acc c -> acc + c + 1) 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  let heightmap = (linesToHeightmap . lines) input
  print $ getLowPoints heightmap