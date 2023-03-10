{-# Language FlexibleContexts #-}

module Hangman where

import Control.Monad.State
import Data.Char
import Data.List
import System.IO
import Text.Printf

type Secret  = String
type Chances = Int
type Guess   = Char
type Options = String

data Hangman = Hangman { hWord :: [(Char, Char)], hChances :: Chances, hOptions :: Options }
  deriving Show

getGuess :: IO Char
getGuess =
  printf "Enter the letter: " >>
  getLine >>= \s -> 
  let x = head s in
  case isAlpha x of
    True  -> do
      return (toUpper x)
    False -> do
      printf "Invalid character\n"
      getGuess

mkHangman :: Secret -> Hangman
mkHangman secret = Hangman (map (\c -> (toUpper c, '_')) secret) 7 ['A'..'Z']

procLetter :: Guess -> StateT Hangman IO ()
procLetter guess = do
  oldHangman <- get
  let word    = hWord oldHangman
      options = hOptions oldHangman
  case not $ elem guess options of
    True  -> do
      liftIO (printf "The letter %s was already guessed! " (show $ toUpper guess))
      liftIO (printf "Choose from following available letters:\n%s\n" options)
    False -> do
      case elem guess (map fst word) of
        True  -> changeWord
        False -> decChances
      where
      decChances = modify (\h -> h { hChances = hChances h - 1 })
      changeWord = modify $ \h ->
        let oldWord    = hWord h
            newWord    = map (\(secret, y) -> if secret == guess then (secret, guess) else (secret, y)) oldWord
            oldOptions = hOptions h
            newOptions = filter (\l -> l /= guess) oldOptions
        in  h { hWord = newWord, hOptions = newOptions }

renderGame :: StateT Hangman IO ()
renderGame = do
  hangman <- get
  let word    = hWord hangman
      chances = hChances hangman
      options = hOptions hangman
  liftIO (putStrLn (intersperse ' ' (map snd word)))
  liftIO (printf "You have %d chances left.\n" chances)
  
playGame :: StateT Hangman IO ()
playGame = do
  h <- get
  case hChances h > 0 of
    False -> do
      liftIO (printf "Sorry, You lose!")
    True  -> do
      case all (/= '_') (map snd (hWord h)) of
        True  -> do
          liftIO (printf "The word was %s.\n" (map snd (hWord h)))
          liftIO (printf "Yay, You win!\n\n")
        False -> do
          renderGame
          guess <- liftIO getGuess
          procLetter guess
          playGame

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  printf "Enter the secret word: "
  hSetEcho stdin False
  secret <- getLine
  hSetEcho stdin True
  printf "\n"
  runStateT playGame (mkHangman secret)
  return ()