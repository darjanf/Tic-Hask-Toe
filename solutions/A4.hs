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
isWinningLine_ player line = null (filter (\sq -> player /= sq) line)

-- *** Assignment 4-2 *** --

-- Q#07
isWinningLine :: Player -> (Line -> Bool)
isWinningLine _ [] = False
isWinningLine player line = foldr (\sq r -> player == sq && r) True line

-- Q#08
hasWon :: Player -> (Board -> Bool)
hasWon _ [] = False
hasWon player board = foldr (\line r -> isWinningLine player line || r) False (getAllLines board)

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09

getGameState = undefined


playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined