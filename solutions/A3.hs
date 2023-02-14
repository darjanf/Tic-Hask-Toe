module A3 where

import A1
import A2

import Data.List (transpose)
import Data.Monoid (All(getAll))
import Control.Concurrent (yield)

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_' :: String
_HEADER_' = " " ++ formatLine (showInts _RANGE_)

-- Q#02
showSquares' :: [Square] -> [String]
showSquares' []     = []
showSquares' (x:xs) = show x : showSquares' xs


-- Q#03
formatRows' :: [Row] -> [String]
formatRows' [] = []
formatRows' (x:xs) = formatLine (showSquares' x) : formatRows' xs

-- Q#04
isColEmpty :: Row -> (Int -> Bool)
isColEmpty [] _     = False
isColEmpty (x:_) 0  = x == EMPTY
isColEmpty (_:xs) i = isColEmpty xs (i - 1)

isColEmpty' :: Row -> (Int -> Bool)
isColEmpty' [] _ = False
isColEmpty' r i = isInBound i && y == EMPTY where (x,y:ys)= splitAt i r

-- Q#05
dropFirstCol' :: Board -> Board
dropFirstCol' []      = []
dropFirstCol' (x:xs)  = tail x : dropFirstCol' xs

dropLastCol' :: Board -> Board
dropLastCol' []     = []
dropLastCol' (x:xs) = init x : dropLastCol' xs

-- Q#06
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (x:xs) = head x : getDiag1 (dropFirstCol' xs)

getDiag2 :: Board -> Line
getDiag2 [ ]= []
getDiag2 (x:xs) = last x : getDiag2 (dropLastCol' xs)

getAllLines :: Board -> [Line]
getAllLines [] = []
getAllLines b = concat [b, transpose b, [getDiag1 b], [getDiag2 b]]

-- *** Assignment 3-2 ***

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p b (r,c)
    | isInBound r = concat [x, [replaceSquareInRow p c y], ys]
    | otherwise   = b
    where 
        (x,y:ys) = splitAt r b

-- Q#08
prependRowIndices' :: [String] -> [String]
prependRowIndices' []     = []
prependRowIndices' s = workerFunction (indexRowStrings s)
    where
        workerFunction :: [(Char,String)] -> [String]
        workerFunction [] = []
        workerFunction ((i,str):hs) = (i : str) : workerFunction hs

-- Q#09
isWinningLine' :: Player -> (Line -> Bool)
isWinningLine' _ [] = False
isWinningLine' p l = workerFunction l
    where
        workerFunction :: Line -> Bool
        workerFunction []     = True 
        workerFunction (x:xs) = (x == p) && workerFunction xs

-- Q#10
isValidMove :: Board -> (Move -> Bool)
isValidMove [] _ = False
isValidMove b m = isMoveInBounds m && workerFunction b m
    where
        workerFunction :: Board -> (Move -> Bool)
        workerFunction [] _ = True
        workerFunction (x:_) (0,c) = isColEmpty' x c
        workerFunction (_:xs) (r,c) = workerFunction xs (r-1,c)