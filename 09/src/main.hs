import Data.Char (digitToInt)
import Data.List (find)
import Data.Maybe

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

linesToHeightmap :: [String] -> [[Int]]
linesToHeightmap = map (map digitToInt)

isOutOfBounds :: [[Int]] -> (Int, Int) -> Bool
isOutOfBounds matrix (x, y) = x < 0 || y < 0 || x >= length matrix || y >= length (matrix !! x)

findSurrounding :: [[Int]] -> (Int, Int) -> [Maybe Int]
findSurrounding matrix (x, y) = map (getElementAt matrix) positions
  where
    positions = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getElementAt :: [[Int]] -> (Int, Int) -> Maybe Int
getElementAt matrix point@(x, y) = if isOutOfBounds matrix point then Nothing else Just ((matrix !! x) !! y)

isPointLowest :: Int -> [Maybe Int] -> Bool
isPointLowest x = all f
  where
    f (Just n) = n > x
    f Nothing = True

getLowPoints :: [[Int]] -> [Int]
getLowPoints matrix =
  foldl (\acc (x, row) -> acc ++ getLowPointsFromRow x row) [] (enumerate matrix)
  where
    getLowPointsFromRow xa row =
      map snd (filter (\(ya, point) -> isPointLowest point (findSurrounding matrix (xa, ya))) (enumerate row))

floodFill :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> Int
floodFill matrix point@(x, y) visited =
  if value < 9 && notVisited then 1 + foldl (\acc p -> acc + floodFill matrix p (point : visited)) 0 positions else 0
  where
    value =
      case getElementAt matrix point of
        Nothing -> 9
        (Just z) -> z
    positions = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    notVisited = isNothing (find (== point) visited)

replaceMaxBasin :: (Int, Int, Int) -> Int -> (Int, Int, Int)
replaceMaxBasin allb@(b1, b2, b3) bn
  | b1 < bn = (bn, b2, b3)
  | b2 < bn = (b1, bn, b3)
  | b3 < bn = (b1, b2, bn)
  | otherwise = allb

getLargestBasins :: [[Int]] -> (Int, Int, Int)
getLargestBasins matrix =
  foldl (\acc (x, row) -> foldl replaceMaxBasin acc (getBasins x row)) (0, 0, 0) (enumerate matrix)
  where
    getBasins xa row =
      map (\(ya, point) -> if isPointLowest point (findSurrounding matrix (xa, ya)) then floodFill matrix (xa, ya) [] else 0) (enumerate row)

sumLowPoints :: [Int] -> Int
sumLowPoints = foldl (\acc c -> acc + c + 1) 0

main :: IO ()
main = do
  input <- readFile "test.txt"
  let heightmap = (linesToHeightmap . lines) input
  let (b1, b2, b3) = getLargestBasins heightmap
  print (b1, b2, b3)
  print (b1 * b2 * b3)
  print $ sumLowPoints $ getLowPoints heightmap