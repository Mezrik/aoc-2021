-- https://adventofcode.com/2021/day/4
import Data.Char (isSpace)
import Data.List (transpose, find)
import Data.List.Split (splitOn)
import Data.Maybe

type Board = [[(Maybe Int)]]

constructRow :: String -> [(Maybe Int)]
constructRow = map (\y -> (Just y)) . ((map read) . words)

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

updateBoard :: Int -> Board -> Board 
updateBoard x = map (map (mark (Just x)))
  where mark x y | x == y = Nothing
                 | otherwise = y

checkBoardWin :: Board -> Bool
checkBoardWin x = check x || check (transpose x)
  where
    check = any (\y -> all isNothing y)

checkWin :: [Board] -> Maybe Board
checkWin = find checkBoardWin

getNumber :: Maybe Int -> Int
getNumber (Just x) = x
getNumber Nothing = 0

calcScore :: (Int, Maybe Board) -> Int
calcScore (x, Just ys) = sum (map (\y -> sum (map getNumber y)) ys) * x
calcScore _ = 0

play :: [Int] -> [Board] -> Int
play [] zs = 0
play (x:xs) zs = if isNothing winner then play xs updated else calcScore (x, winner)
  where 
    updated = (map (\z -> updateBoard x z) zs)
    winner = checkWin updated

playWithLastWin :: [Int] -> [Board] -> [(Int, Maybe Board)] -> Int
playWithLastWin _ [] winners = calcScore (last winners)
playWithLastWin [] _ [] = 0
playWithLastWin (x:xs) zs winners = if null xs 
  then calcScore (last newWinners)
  else playWithLastWin xs (filter (not . checkBoardWin) updated) newWinners
  where 
    updated = (map (\z -> updateBoard x z) zs)
    newWinners = case (checkWin updated) of
      Nothing -> winners
      y -> winners ++ [(x, y)]
 
main :: IO ()
main = do
  input <- readFile "input.txt"
  let (toBeDrawn:_:rest) = lines input
  let score = play (map read (splitOn "," toBeDrawn)) (constructBoards rest)
  let score2 = playWithLastWin (map read (splitOn "," toBeDrawn)) (constructBoards rest) []
  -- First assignment
  print $ score
  -- Second assignment
  print $ score2