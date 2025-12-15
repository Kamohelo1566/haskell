{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import System.Random (randomRIO)
import Data.List (group, sort)
import Control.Exception
import System.IO

-- -----------------------
-- HC14T5: Custom Data Type with @ pattern matching
-- -----------------------
data Result a = Success a | Failure String deriving Show

processResult :: Result Int -> String
processResult r@(Success n) = "Got Success: " ++ show n ++ " in " ++ show r
processResult (Failure msg) = "Failure: " ++ msg

-- -----------------------
-- HC14T8: Character frequency function
-- -----------------------
counts :: String -> [(Char, Int)]
counts s = map (\g -> (head g, length g)) . group . sort $ s

-- -----------------------
-- HC14T4: TypeApplications function to read string as Int
-- -----------------------
stringToInt :: String -> Int
stringToInt s = read @Int s

-- -----------------------
-- HC14T3: Numeric underscores
-- -----------------------
bigNumber :: Integer
bigNumber = 1_000_000_000

-- -----------------------
-- Main Function: Demonstrates all tasks
-- -----------------------
main :: IO ()
main = do
    -- HC14T1: Hello Cabal (simulating cabal executable)
    putStrLn "Hello, Cabal!"

    -- HC14T2: Print random number (1-100)
    rand <- randomRIO (1,100) :: IO Int
    putStrLn $ "Random number (1-100): " ++ show rand

    -- HC14T3: Numeric underscores
    putStrLn $ "Big number with underscores: " ++ show bigNumber

    -- HC14T4: TypeApplications
    let num = stringToInt "12345"
    putStrLn $ "String converted to Int using TypeApplications: " ++ show num

    -- HC14T5: Custom data type & pattern matching
    putStrLn $ processResult (Success 42)
    putStrLn $ processResult (Failure "Error occurred")

    -- HC14T6: Project structure demonstration
    putStrLn "Project structure: src/ and app/ directories (simulated)"

    -- HC14T7: Library component demonstration (simulated)
    putStrLn "Library component: counts function from 'library module'"

    -- HC14T8: Character frequency
    let freq = counts "hello world"
    putStrLn "Character frequencies in 'hello world':"
    print freq

    -- HC14T9: PartialTypeSignatures demonstration
    let _x :: _ = 42
    putStrLn $ "PartialTypeSignatures example value: " ++ show _x

    -- HC14T10: Test suite demonstration (simulated)
    putStrLn "Test suite for counts function (simulated):"
    if counts "aab" == [('a',2),('b',1)]
       then putStrLn "Test passed!"
       else putStrLn "Test failed!"
