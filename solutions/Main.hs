module Main where

import A1
import A2
import A3
import A4
import A5
import Playground
import Data.List
import Data.Ord
import Text.Printf
import Control.Monad

-- Task: Prompt the user for a string and print it.

main :: IO ()
{-
main = do -- do only needed if "<-" is used. If "<-" not used, we don't need "do" here
    --putStrLn "Welcome to Part I of EMURGO Academy's Haskell course!"
    --putStrLn "Please type in your first name:" >>= \() -> 
        -- here the () is the void return object
        -- you can also just use ">>" instead of ">>= \() ->"
    --getLine >>= (\s -> putStrLn ("Hey, " ++ s))

    putStrLn "Please type in your first name:"
    --putStrLn "Please type in your first name:" >>

    s <- getLine
    --getLine >>= \s -> -- in "s" the input is saved
    
    putStrLn "Please type in your last name:"
    --putStrLn "Please type in your last name:" >> 
   
    t <- getLine
    --getLine >>= \t -> 
    
    putStrLn ("Hey, " ++ s ++ " " ++ t)
-- to convert IO String to String you can think of a 'promise'
-- a promise is not really a string, you can call the promise and it will return a string,
-- as soon as the string is there
-}

{-
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    putStrLn ("Longest: " ++ maximumBy (comparing length) [a,b,c])
    putStrLn ("Shortest: " ++ minimumBy (comparing length) [a,b,c])
-}

{-
main = 
    getLine >>= \a ->
    getLine >>= \b ->
    getLine >>= \c ->
    let maximum = foldr1 (\cur max -> if length cur > length max then cur else max) [a,b,c]
        minimum = foldr1 (\cur max -> if length cur < length max then cur else max) [a,b,c]
    in putStrLn ("longest: " ++ maximum) >>
       putStrLn ("shortest: " ++ minimum)
-}

-- use "read" function

--Task:
-- 1. Prompt the user for two numbers and print their sums

{-
main = 
    getLine >>= \a ->
    getLine >>= \b ->
    let num1 = read a :: Int
        num2 = read b :: Int
        sum = num1 + num2
    -- in putStrLn ("sum: " ++ show sum)
    in print sum -- u can use print istead of putStrLn
-}

{-
getNumber :: IO Int
getNumber = getLine >>= \a -> return (read a)
main = 
    getNumber >>= \a ->
    getNumber >>= \b ->
    print (a + b)
-}

{-
getNumber :: IO Float
getNumber = getLine >>= \a -> return (read a)

readInputs :: Int -> IO [Float]
readInputs 0 = return []
readInputs n = 
    getNumber >>= \x ->
    readInputs (n-1) >>= \xs ->
    return (x : xs)

average :: Int -> IO ()
average n = 
    readInputs n >>= \zs -> 
    printf "Average: %f\n" (sum zs / fromIntegral (length zs))

main = average 3
-}

{-
average :: Int -> IO ()
average n = 
    replicateM n getLine >>= \xs ->
    let
        ns :: [Float]
        ns = map read xs
    in  print (sum ns / fromIntegral (length ns))

main = average 2
-}
-- main = firstPlayer >>= \p -> play _EMPTY_BOARD_ p
main = do
    p <- firstPlayer
    playDo _EMPTY_BOARD_ p