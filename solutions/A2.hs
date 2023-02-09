{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> String
promptPlayer x = concat ["Player ", showSquare x, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

readDigit :: Char -> Int
readDigit x
    | isDigit x = read [x]
    | otherwise = -1

-- Q#04
_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ EMPTY

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied b = EMPTY `notElem` concat b


_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
      [X, O, O]
    , [O, X, X]
    , [O, X, O]
    ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings x = zip ['A'..] x

-- Q#07
formatLine :: [String] -> String
formatLine x = concat [_SEP_, intercalate _SEP_ x, _SEP_]

-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (row,column) = isInBound row && isInBound column

isInBound :: Int -> Bool
isInBound x = x >= 0 && x < _SIZE_

-- Q#09
stringToMove :: String -> Move
stringToMove [x,y] = (convertRowIndex x, readDigit y)
stringToMove _     = _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> (Int -> (Row -> Row))
replaceSquareInRow player index row
    | null row = row
    | isInBound index && isFieldEmpty y = x ++ player:ys
    | otherwise = row
    where
        (x,y:ys)= splitAt index row

isFieldEmpty :: Square -> Bool
isFieldEmpty y = y == EMPTY

rsX :: Int -> (Row -> Row)
rsX index row = replaceSquareInRow X index row

rsO :: Int -> (Row -> Row)
rsO index row = replaceSquareInRow O index row