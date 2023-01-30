--module A1 where

import Data.Char (toUpper)

_MY_PAIR_ :: (Int, Bool)
_MY_PAIR_ = (33, True)

_TRIPLE_ :: (Int, Bool, Char)
_TRIPLE_ = (22, False, 'j')

_FIB_ :: [Int]
_FIB_ = [33,22, 11]

_FAV_POKEMON_ :: String
_FAV_POKEMON_ = "Alakazam"

type Id = Int
type Quantity = Int
type Name = String

_PIKACHU_ID_ :: Id
_PIKACHU_ID_ = 25

_PARTY_SIZE_ :: Quantity
_PARTY_SIZE_ = 6

pikachu :: (String, Int)
pikachu = ("Pikachu", 25)

description :: (String, Int) -> String
description (name, number) = name ++ " is " ++ show number ++ " years old!"

description_ :: (String, Int) -> String
description_ p = fst p ++ " is " ++ show (snd p) ++ " years old!"

third :: [a] -> a
third (_:_:c:_) = c

isNull :: [a] -> Bool
isNull xs = null xs 

isNull' :: [a] -> Bool
isNull' xs = length xs == 0 

isNull'' :: [a] -> Bool
isNull'' []       = True
isNull'' (x : xs) = False

isNull''' :: [a] -> Bool
isNull''' []       = True
isNull''' _        = False

isNull'''' :: [a] -> Bool
isNull'''' xs
    | null xs       = True
    | otherwise     = False

isSingleton :: [a] -> Bool
isSingleton (x : []) = True
isSingleton _        = False

isSingleton' :: [a] -> Bool
isSingleton' [x] = True
isSingleton' _   = False

isSecondLetterT :: String -> Bool
isSecondLetterT (_ : 't' : _) = True
isSecondLetterT _             = False

last' :: [a] -> a
last' []       = error "empty list"
last' [x]      = x
last' (_ : xs)  = last' xs