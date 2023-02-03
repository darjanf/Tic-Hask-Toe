import Data.Char
import Debug.Trace

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

-- Curry
append :: String -> (String -> String)
append x y = x ++ y
prepend_abc = append "abc" -- append "abc" returns a function object (String -> String)
_FULL_STRING_ = prepend_abc "xyz" -- calling the return function
_FULL_STRING_2_ = append "abc" "xyz" -- calling the append function directly with 2 arguments

-- Partial Application
type Pokemon = String
type Move    = String

printMove :: Pokemon -> Move -> String
printMove p m = p ++ " used " ++ m ++ "!"
-- this is the saturated function calling form:
-- printMove "Pickachu" "Thunderbolt" 
-- here, function ist not fully satured, only first argument is taken and return a function object:
pickachuMove = printMove "Pickachu" -- partial Application form of calling an function
-- pickachuMove "Thunderbolt" -- function printMove gets now fully saturated
-- output: "Pickachu used Thunderbolt!"

and' :: Bool -> (Bool -> Bool)
and' True True = True
and' _ _ = False

isListShort :: [a] -> Bool
isListShort xs = length xs < 4

isListLong :: [a] -> Bool
isListLong xs = not (length xs < 4)

-- Lexical Scope using where
greet :: String -> String
greet name = "Welcome to Viridian City, " ++ name' ++ "!"
    where
        name' = toUpper (head name) : tail name

-- Lexical Scope using let
greet' :: String -> String
greet' name = 
    let name' = toUpper (head name) : tail name
    in "Welcome to Viridian City, " ++ name' ++ "!"

fizzbuzz :: Int -> String
fizzbuzz x
    | isDivisible x 3 && isDivisible x 5 = "FizzBuzz"
    | isDivisible x 3                    = "Fizz"
    | isDivisible x 5                    = "Buzz"
    | otherwise                          = show x
    where 
        isDivisible :: Int -> Int -> Bool
        isDivisible x y = mod x y == 0

-- Recursion
last' :: [a] -> a
last' []       = error "empty list"
last' [x]      = x
last' (_ : xs)  = last' xs

reverse' :: [a] -> [a]
reverse' [] = [] -- BASE CASE
reverse' (x : xs) = reverse' xs ++ [x] -- RECURSIVE CASE

reverseWithLoop :: [a] -> [a]
reverseWithLoop ys = loop [] ys 
    where
        loop :: [a] -> [a] -> [a]
        loop result []       = result
        loop result (x : xs) = loop (x : result) xs

length' :: [a] -> Int
length' [] = 0 -- BASE CASE
length' (x : xs) = 1 + length' xs -- RECURSIVE CASE

-- In Haskell there is no Count (++) possibility for a variable
-- but when you use a helper function to simulate a counter:
lengthWithLoop :: [a] -> Int
lengthWithLoop xs = loop 0 xs
    where
        loop :: Int -> [a] -> Int
        loop count []       = count
        loop count (x : xs) = loop (count + 1) xs

isPrime :: Int -> Bool
isPrime z = loop 2 z
    where
        loop 1 x = False
        loop 2 x = True
        loop i x
            | i == x - 1 = True
            | otherwise  = if mod x i == 0 then False else loop (i + 1) x

elem' :: Eq a => a -> [a] -> Bool
elem' x []       = False
elem' x (y : ys) = x == y || elem' x ys