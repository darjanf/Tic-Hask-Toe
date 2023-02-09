module Playground where

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

--map
upper :: String -> String
upper []     = []
upper (x:xs) = toUpper x : upper xs

-- map' :: (a -> b) -> [a] -> [b] 
-- (a -> b) is the first argument
-- [a] is second argument

upperS :: String -> String
upperS = map toUpper
-- or 
-- upperS xs = map toUpper xs

--filter
removeSymbol :: String -> String
removeSymbol [] = []
removeSymbol (x:xs)
    | (isAlpha x) = x : removeSymbol xs
    | otherwise   = removeSymbol xs

double :: Int -> Int
double x = x * 2

ys :: [Int]
ys = [ 7, 1, 8, 12 ]

-- calling map
-- map double ys

lenghts :: [String] -> [Int]
lenghts = map length
-- calling lenghts
-- lenghts ["aaa", "bbbb", "ccccc"]

--filter to remove chars if it is not a consonstant
s :: String
s = "lorem ipsum dolor sit amet"
isConsonant :: Char -> Bool
-- isConsonant c = isAlpha c && not (elem c "aeiou")
isConsonant = \c -> isAlpha c && not (elem c "aeiou")
s' = filter isConsonant s -- calling filter
s'' = filter (\c -> isAlpha c && not (elem c "aeiou")) s -- calling filter without using functionname

-- Lambda Expression
-- a general representation of a function
-- (\x -> ...) 7
-- x is the argument; ... is the filter; 7 is the number used when filter is used
-- (\x -> x+2) 3 
-- result: 5
addTwo = \x -> x + 2
-- addTwo 2 --> result = 4
addTwoResult = addTwo 3
-- addTwoResult = 5

-- using map without function
j = map (\x -> x^2) [1,2,3,4,5]
-- result = [1,4,9,16,25]
h = map (\x -> 2^x) [1,2,3,4,5]
-- result = [2,4,8,16,32]
-- Operator Section
-- (+ 3)
-- \x -> x + 3 
-- (* 3)
-- \x -> x * 3
-- prefix always with () surrounded
-- infix always with `` surrounded

map' :: (a -> b) -> [a] -> [b]
map' _ []        = []
map' f (x:xs)    = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

--isConsonant = \c -> isAlpha c && not (elem c "aeiou")
-- filter (isUpper) "AbC"
-- result: "AC"