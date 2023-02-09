module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01
_HEADER_ :: String
_HEADER_ = " " ++ formatLine (map show _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares sq = map show sq

-- Q#03
dropFirstCol :: Board -> Board
dropFirstCol b = map tail b

-- Q#04
dropLastCol :: Board -> Board
dropLastCol b = map init b

--Q#05
formatRows :: [Row] -> [String]
formatRows rl = map (\x -> formatLine (showSquares x)) rl
--formatRows rl = map (formatLine . showSquares) rl

-- Q#06
isWinningLine_ :: Player -> (Line -> Bool)
isWinningLine_ _ [] = False
isWinningLine_ p l = null (filter (\sq -> p /= sq) l)

-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine = undefined

-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined


playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined