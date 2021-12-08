-- https://adventofcode.com/2021/day/5
import Data.List.Split (splitOn)
import Data.List (unfoldr, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe


type Point = (Int, Int)
type Line = (Point, Point)

tuplify :: [String] -> (Int, Int)
tuplify [x, y] = (read x, read y)

constParseRecords :: [String] -> [Line]
constParseRecords = map (getPoint . parseLine)
  where 
    getPoint [x, y] = ((tuplify . splitOn ",") x, (tuplify . splitOn ",") y)
    parseLine line = splitOn "->" line

getValues :: [Line] -> ([Int], [Int])
getValues [] = ([], [])
getValues (((x1, y1), (x2, y2)):xs) = ((x1 : x2 : fst zs), (y1 : y2 : snd zs))
  where 
    zs = getValues xs

bresenham :: Line -> [Point]
bresenham (pa@(xa, ya), pb@(xb, yb)) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)

writeLine :: Map Point Int -> Line -> Map Point Int
writeLine linesMap line = foldl updateMap linesMap entireLine
  where
    entireLine = bresenham line
    updateMap acc c = 
      if Map.member c acc then Map.update (\z -> Just (z + 1)) c acc else Map.insert c 1 acc


writeLines :: [Line] -> Map Point Int
writeLines xs = foldl writeLine (Map.fromList []) xs

countOverlaps :: Map Point Int -> Int
countOverlaps = foldl (\acc c -> if c >= 2 then acc + 1 else acc) 0

filterDiagonal :: [Line] -> [Line]
filterDiagonal = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedLines = constParseRecords  $ lines input
  let (xs, ys) = getValues parsedLines
  let linesMapWithoutDiagonal = writeLines $ filterDiagonal parsedLines
  let linesMapWithDiagonal = writeLines parsedLines
  -- First assignment
  print $ countOverlaps linesMapWithoutDiagonal
  -- Second assignment
  print $ countOverlaps linesMapWithDiagonal
