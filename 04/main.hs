-- https://adventofcode.com/2021/day/4
import Data.Map (Map, fromList, update, elems, keys)
import qualified Data.Map (filter)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.Maybe
import Data.List (find)

type Row = Map Int Bool
type Board = [Row]

constructRow :: String -> Row
constructRow x = fromList $ (map (\y -> (y, False)) . ((map read) . words)) x

constructBoard :: [String] -> ([String], Board)
constructBoard (y:ys) 
  | all isSpace y = (ys, [])
  | otherwise = (st, constructRow y : nd)
      where (st, nd) = constructBoard ys
constructBoard [] = ([], [])

constructBoards :: [String] -> [Board]
constructBoards [] = []
constructBoards xs = nd : constructBoards st
  where (st, nd) = constructBoard xs 

drawNumber :: [Int] -> (Int, [Int])
drawNumber x = (head x, drop 1 x)

updateBoard :: Int -> Board -> Board 
updateBoard x = map (update (\_ ->(Just True)) x)

checkWin :: [Board] -> Maybe Board
checkWin = find checkBoardWin 
  where checkBoardWin = any (\y -> all id (elems y))

calcScore :: Int -> Maybe Board -> Int
calcScore x (Just ys) = sum (map (\y -> sum (keys (Data.Map.filter not y))) ys) * x
calcScore _ _ = 0

play :: [Int] -> [Board] -> Int
play [] zs = 0
play xs zs = if isNothing winner then play ys updated else calcScore y winner
  where 
    (y,ys) = drawNumber xs
    updated = (map (\z -> updateBoard y z) zs)
    winner = checkWin updated
 
-- First assignment
ptOne :: IO ()
ptOne = do
  input <- readFile "input.txt"
  let (toBeDrawn:_:rest) = lines input
  let score = play (map read (splitOn "," toBeDrawn)) (constructBoards rest)
  print $ score
