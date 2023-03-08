module Hangman where

import Data.Char
import Debug.Trace
import Data.List
import Data.Ord
import Text.Read hiding (get)
import Control.Monad.State
import System.IO

{-
_ _ _ _ _
Task of Hangman is to guess the word, by asking if a letter is inside
if a letter is not inside a word, your wrong-answer-number increased

type Secret     = String
type Chances    = Int
type Guess     = Char

data Hangman = Hangman {
    hWord       :: [(Char, Char)],
    hChances    :: Chances
} deriving Show

mkHangman :: Secret -> Hangman
mkHangman secret = Hangman (map (\c -> (toUpper c, '_')) secret) 7

getGuess :: IO Char
getGuess =
    printf "Enter the letter.\n" >>
    getLine >>= \s ->
    let x = head s in
    case isAlpha $ x of
        True    -> do
            return $ toUpper x
        False   -> do
            printf "Invalid character."
            getGuess

decChances :: State Hangman ()
decChances = modify (\h -> h {hChances = hChances h - 1})

changeWord :: Guess -> State Hangman ()
changeWord guess = modify $ \h ->
    let oldWord = hWord h
        newWord = map (\(x,y) -> if x == guess then (x,guess) else (x, '_')) oldWord
    in h { hWord = newWor }

procLetter :: Guess -> State Hangman ()
procLetter guess = do
    currentHangman <- get
    let word = hWord currentHangman
    case elem guess (map fst word) of
        True    -> changeWord guess
        False   -> decChances

main =
    hSetBuffering stdout NoBuffering >>
    printf "Enter the secret word." >>
    hSetEcho stdin False >>
    getLine >>= \secret ->
    hSetEcho stdin True >>
    printf "\n" >>
    print (mkHangman secret)
    --let hangman = mkHangman secret
    --putStrLn "You have %d chances.\n" (hChances hangman)
    --printf "Enter the letter.\n"
    -- letter <- getLetter
    --printf "you have entered %c\n." letter

-}