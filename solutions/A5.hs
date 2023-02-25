module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01
printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= \a -> putStrLn a

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= \b -> return $ getFirstPlayer b

-- Q#04
getMove :: Board -> IO Move
getMove b =
    getLine >>= \m ->
    let move = stringToMove m
        vm = isValidMove b move
    in 
        if vm then return move else putStrLn "Invalid move! Try again" >> getMove b

-- Q#05
play :: Board -> Player -> IO ()
play b p =
    when _DISPLAY_LOGO_ printLogo >>
    printBoard b >>
    print (promptPlayer p) >>
    getMove b >>= \m ->
    let (gamestate, newboard) = playMove p b m
    in 
        if gamestate == IN_PROGRESS
        then play newboard $ switchPlayer p
        else 
            printBoard newboard >>
            print (showGameState gamestate)

-- *** Assignment 5-2 *** --

-- Q#07
printLogoDo :: IO ()
printLogoDo = do
    a <- readFile _LOGO_PATH_
    putStrLn a

-- Q#08
firstPlayerDo :: IO Player
firstPlayerDo = do
    b <- _RANDOM_BOOL_
    return $ getFirstPlayer b

-- Q#09
getMoveDo :: Board -> IO Move
getMoveDo b = do
    m <- getLine
    let move = stringToMove m
        vm = isValidMove b move
    --in
    if vm 
    then do return move 
    else do 
        putStrLn "Invalid move! Try again"
        getMove b

-- Q#10
playDo :: Board -> Player -> IO ()
playDo b p = do
    when _DISPLAY_LOGO_ printLogo
    printBoard b
    print (promptPlayer p)
    m <- getMove b
    let (gamestate, newboard) = playMove p b m
    if gamestate == IN_PROGRESS
    then do play newboard $ switchPlayer p
    else do
        printBoard newboard
        print (showGameState gamestate)