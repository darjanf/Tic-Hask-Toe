module Playground where

import Data.Char
import Debug.Trace
import Data.List
import Data.Ord
import Text.Read hiding (get)
import Control.Monad.State

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
-- filter' (isUpper) "AbC"
-- result: "AC"

-- fold
-- fold is used for accumulation, where you change the output value
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs
-- sum' [1,2,3]
product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs

-- the difference between sum' and product' is just 
-- the exit expression (0,1,...) and the operator (+,*,...)
-- use fold instead of sum'
-- foldr (\x y -> x + y) 0 [1 .. 7]

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr x ys = foldr (\a b -> a == x || b) False ys
-- Eingabe: elemFoldr 19 [1,2,3,4,5,6,7]
-- x = 19
-- ys = [1,2,3,4,5,6,7]
-- a = 1 // head of ys
-- b /= [2,3,4,5,6,7]
-- b = elemFoldr 19 [2,3,4,5,6,7] // function called on tail of ys

-- 1st call = foldr (\a b -> a == 'h' || b) False "ghi"
-- 2nd call = (\a b -> a == 'h' || b) 'g' (foldr f False "hi")

reverseFoldr :: [a] -> [a]
-- reverseFoldr []        = []
-- reverseFoldr (x : xs) = reverseFoldr xs ++ [x]
reverseFoldr ys = foldr (\a b -> b ++ [a]) [] ys

upperFolder :: String -> String
upperFolder ys = foldr (\a b -> (toUpper a) : b) [] ys

-- old implementation of filter'
--filter' :: (a -> Bool) -> [a] -> [a]
--filter' p []     = []
--filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs
-- new implementation of filter' with foldr
filterFolder :: (a -> Bool) -> [a] -> [a]
filterFolder p z = foldr (\x ys -> if p x then x : ys else ys) [] z
-- if p x then x : filter' p xs else filter' p xs

pokemonList :: [(String,Int)]
pokemonList = [("Bulb", 1), ("Char", 4), ("Squirtle", 7), ("Pikachu", 25)]
-- expected result ["Bulb", "Char", "Squirtle", "Pickachu"]
getFirstItemOfTupel :: [(String,Int)] -> [String]
getFirstItemOfTupel ys = foldr (\(s,i) y -> s : y) [] ys
-- function call = getFirstItemOfTupel pokemonList
-- result ["Bulb", "Char", "Squirtle", "Pickachu"]
sumSecondItemOfTupel :: [(String,Int)] -> Int
sumSecondItemOfTupel ys = foldr (\(s,i) y -> i + y) 0 ys
-- sumSecondItemOfTupel ys = foldr (\x y -> snd x + y) 0 ys
-- function call = sumSecondItemOfTupel pokemonList
-- result: 37

-- foldl
-- fold (\y x -> 1 + y) 0 "abc"
-- result: 3

--pangram :: String -> Bool
--pangram l = 
 --   let s' = map toUpper s
 --   in and (\a -> elem a s') ['A'..'Z']

toUpperTest1 = map (toUpper $) "abc" ++ "def" --result: ABCDEF
toUpperTest2 = map toUpper $ "abc" ++ "def" --result: ABCdef

-- Composition
f :: String -> Char
f = toUpper . head -- head will be called first (right to left:)

(&) :: (a -> b) -> a -> b
(&) f p = f p

{-
data Pokemon' = MkPokemon String Int [String]
    deriving Show
-}
--type Name = String
type Number = Int
type Power = String
data Pokemon' = MkPokemon -- record syntax; MkPokemon is the constructorName
    {
        pName :: Name, -- these attributes are global getter/setter functions
        pNumber :: Number,
        pPowers :: [Power]
    } deriving (Eq)

mybulbasauer :: Pokemon'
-- bulbasauer = MkPokemon "Bulbasauer" 1 ["Grass", "Poison"]
mybulbasauer = MkPokemon { 
    pName =   "Bulbasauer",
    pNumber = 1,
    pPowers = ["Grass", "Poison"]
}
mypikachu = MkPokemon "Pikachu" 25 ["Electric"]

getName :: Pokemon' -> String
getName (MkPokemon name _ _) = name

-- for chaning pPowers:
-- bulbasauer' = bulbasauer { pPowers = ["Grass", "Poison"]} -- this creates a new Pokemon
bulbasauer' = 
    let oldPowers = pPowers mybulbasauer -- here pPowers is used as a getter function
        newPowers = oldPowers ++ ["Poison"]
    in mybulbasauer { pPowers = newPowers } -- here pPowers is used as a setter function

maxNumber :: [Pokemon'] -> Pokemon'
--maxNumber ps = maximumBy (comparing pNumber) ps
maxNumber ps =
    foldr1 (\p q -> if pNumber p > pNumber q then p else q) ps

-- Polymorphic Types -- synonym is parametric datatype
-- data Pair a b = MkPair a b
data Pair a b = MkPair { first :: a, second :: b } -- record syntax
    deriving Show

p2t :: Pair a b -> (a,b)
p2t (MkPair x y) = (x,y)

t2p :: (a,b) -> Pair a b
t2p (x,y) = MkPair x y

-- p2t and t2p are called semimorphic types

-- Optional datatype
-- Int' is almost a Int but without an extra Null value called Nil
data Int' = MkInt' Int | INil
    deriving Show
xInt' :: Int'
xInt' = MkInt' 7
yInt :: Int
yInt = 7
-- divide :: Int -> Int -> Int
-- if you use this signature, then you get a exception when dividing by 0
-- instead use Int', because we defined a Null value called INil
divide :: Int -> Int -> Int'
divide x 0 = INil -- Here you can use the Null value INil
divide x y = MkInt' (div x y) -- but hen you have to use also the MkInt' constructor
-- Optional Dataytpye for polymorphic datatypes
data Optional a = Nil | MkOptional a -- in Haskell there is already a Optional type called maybe
    deriving Show
divide' :: Int -> Int -> Optional Int
divide' x 0 = Nil
divide' x y = MkOptional (div x y)

-- data Maybe already exists in Haskell
-- data Maybe a = Nothing | Just a
--  deriving Show
-- divide :: Int -> Int -> Maybe Int
-- divide x 0 = Nothing
-- divide x y = d

-- ************************************
-- what purpose of whole "maybe" thing?
-- data Maybe a = Just a | Nothing
-- Maybe is used for "partial functions"
-- these function work only on a partial input (f.e. the function "root" or "divide")
divide'' :: Int -> Int -> Maybe Int
divide'' x 0 = Nothing
divide'' x y = Just (div x y)

fDivide :: (Int,Int) -> (Int,Int) -> Maybe Int
-- fDivide (w,x) (y,z) = divide'' w x + divide'' y z
-- this fDivide implementation will not work, you can't add two maybe values
-- the solutions is to extract the divide output
fDivide (w,x) (y,z) =
    case divide'' w x of
        Nothing -> Nothing
        Just a -> 
            case divide'' y z of
                Nothing -> Nothing
                Just b -> Just (a + b)
-- test: fDivide (12,3) (18,0) --> result = 
-- test: fDivide (12,3) (18,6) --> Just 7

validateName :: String -> Maybe String
validateName s = if not (isUpper (head s)) then Nothing else Just s

validateNum :: String -> Maybe Int
validateNum s = 
    let n = readMaybe s
    in case n of 
        Nothing -> Nothing
        Just x -> if x < 1 then Nothing else Just x

validatePowers :: [String] -> Maybe [String]
validatePowers ps = if null ps then Nothing else Just ps

{-
mkPokemon :: String -> String -> [String] -> Maybe Pokemon'
mkPokemon name number powers =
    case validateName name of
        Nothing -> Nothing
        Just name' ->
            case validateNum number of
                Nothing -> Nothing
                Just number' ->
                    case validatePowers powers of
                        Nothing -> Nothing
                        Just powers' -> Just (MkPokemon name' number' powers')
-}

mkPokemon :: String -> String -> [String] -> Maybe Pokemon'
mkPokemon name num powers =
    validateName name       >>= \name'      -> -- instead you can use the bind operator >>=
    validateNum num         >>= \num'       ->
    validatePowers powers   >>= \powers'    ->
    return (MkPokemon name' num' powers')

-- IO and Maybe are both implementing Monad typeclass
-- (>>=) :: IO  a   -> (a -> IO b)      -> IO b
-- (>>=) :: Maybe a -> (a -> Maybe b)   -> Maybe b

-- String and Pokemon are both implementing Eq typeclass
--(==) :: String  -> String   -> Boolean
--(==) :: Pokemon -> Pokemon  -> Boolean

addNums :: IO ()
addNums =
    getLine >>= \s ->
    getLine >>= \t ->
    let a = read s
        b = read t
    in print ((a :: Int) + b)

{-
contexts in Haskell:
input output:
- IO:
data IO a = ...

error handling:
- Maybe
data Maybe a = ...
- Either
data Either e a = ...

state manipulation:
- State: 
data State s a = ...

dynamic environment:
- Reader
-}

type Party = [Pokemon']

{-
Typicall implementation for Pokemon-Party without state would look like following:

addPokemon :: Pokemon' -> Party -> Party
addPokemon p ps = p : ps

removePokemon :: Pokemon' -> Party -> Party
removePokemon p ps = delete p ps
-}

-- Implementation with state looks like following:
addPokemon :: Pokemon' -> State Party ()
addPokemon p = do
    ps <- get -- gets the current state
    -- get >>= \ps -> -- alternative syntax without do
    put (p : ps) -- sets the new state

removePokemon :: Pokemon' -> State Party ()
removePokemon p = do
    ps <- get
    put (delete p ps)

task :: State Party ()
task = do
    addPokemon mypikachu
    addPokemon mybulbasauer

task2 :: State Party Int
task2 = do
    addPokemon mypikachu
    ps <- get
    return (length ps)

task3 :: State Party Int
task3 = do
    task2
    addPokemon mybulbasauer
    ps <- get
    return (length ps)

task4 :: State Party Int
task4 = do
    ps <- get
    return (length ps)

myRunState =  runState task  [] -- return value = ((),[Bulbasauer,Pikachu])
myRunState2 = runState task2 [] -- return value = (1,[Pikachu])
myRunState3 = runState task3 [] -- return value = (2,[Bulbasauer,Pikachu])
myRunState4 = runState task4 [] -- return value = (0,[])

instance Show Pokemon' where
    show pokemon = pName pokemon

-- Monad (only with parameter):
-- Maybe a      -- Monomorphic
-- [a]          -- Monomorphic
-- Either e a   -- Polymorphic
-- State s a    -- Polymorphic
--
-- No Monad (without parameter):
-- Int
-- Char
--
-- Eq:      (==)    :: a -> a -> Bool
-- Show:    show    :: a -> String
-- Num:     (+)     :: a -> a -> a

{-
class Monad m where
    return :: a -> [] a
    (>>=) :: m a -> (a -> m b) -> m b 
    --"m a" is a monad value. this monad value you can't extract
    --but you can take an expression "(a -> m b)" and extract a by returning "m b"
-}