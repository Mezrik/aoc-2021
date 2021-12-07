-- https://adventofcode.com/2021/day/5
import Data.List.Split (splitOn)
import Data.List (unfoldr, sort)

type Point = (Int, Int)
type Line = (Point, Point)
type Diagram = [[Int]]

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

writePoint :: Point -> Diagram -> Diagram
writePoint (x, y) diagram = map (\(row, i) -> map (\(val, j) -> eval val i j) (enumerate row)) (enumerate diagram)
  where
    enumerate xs = zip xs [0..(length xs)]
    eval val i j = if i == x && j == y then val + 1 else val

writeLines :: [Line] -> Diagram -> Diagram
writeLines [] diagram = diagram
writeLines (x:xs) diagram = writeLines xs updatedDiagram
  where
    line = bresenham x
    updatedDiagram = foldl (\acc c -> acc) diagram line

countOverlaps :: Diagram -> Int
countOverlaps = foldl (\total row -> foldl (\count val -> if val >= 2 then count + 1 else count) total row) 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedLines = constParseRecords  $ lines input
  let (xs, ys) = getValues parsedLines
  let diagram = writeLines parsedLines (replicate (maximum xs) (replicate (maximum ys) 0))
  print diagram
