-- HC13 Practical Tasks: All in One File

{-# LANGUAGE Safe #-}

-- Custom module SumNonEmpty (HC13T4 & HC13T5)
module Main where

import System.Directory
import Data.List
import qualified Data.List as L
import qualified Data.Map as Map
import Control.Exception

-- -----------------------
-- SumNonEmpty Module
-- -----------------------
sumNonEmpty :: [Int] -> Int
sumNonEmpty [] = error emptyListMessage
sumNonEmpty xs = sum xs

emptyListMessage :: String
emptyListMessage = "Error: List must not be empty"

-- -----------------------
-- HC13T1: List Files in Directory
-- -----------------------
listFiles :: IO [FilePath]
listFiles = getDirectoryContents "."

-- -----------------------
-- HC13T2: Filter Files by Substring
-- -----------------------
filterFiles :: String -> [FilePath] -> [FilePath]
filterFiles sub files = filter (isInfixOf sub) files

-- -----------------------
-- HC13T3: Sort and Return Filtered Files
-- -----------------------
sortFilteredFiles :: String -> [FilePath] -> [FilePath]
sortFilteredFiles sub files = sort (filterFiles sub files)

-- -----------------------
-- HC13T6: File Names to Map
-- -----------------------
filesToMap :: [FilePath] -> Map.Map Int FilePath
filesToMap files = Map.fromList (zip [1..] files)

-- -----------------------
-- HC13T8: Qualified Imports Example
-- -----------------------
qualifiedSort :: [Int] -> [Int]
qualifiedSort xs = L.sort xs

-- -----------------------
-- HC13T9: Renaming Module Namespace Example
-- -----------------------
mapExample :: [String] -> Map.Map Int String
mapExample xs = Map.fromList (zip [1..] xs)

-- -----------------------
-- HC13T10 & HC13T7: Main Function
-- -----------------------
main :: IO ()
main = do
    putStrLn "=== Haskell Chapter 13 Practical Tasks ===\n"

    -- HC13T1: List files
    files <- listFiles
    putStrLn "Files in current directory:"
    print files

    -- HC13T2: Filter files containing "hs"
    let filtered = filterFiles "hs" files
    putStrLn "\nFiltered files containing 'hs':"
    print filtered

    -- HC13T3: Sorted filtered files
    let sortedFiltered = sortFilteredFiles "hs" files
    putStrLn "\nSorted filtered files:"
    print sortedFiltered

    -- HC13T6: Convert to Map
    let fileMap = filesToMap filtered
    putStrLn "\nFiles converted to Map:"
    print fileMap

    -- HC13T7: Use custom module SumNonEmpty
    putStrLn "\nUsing SumNonEmpty module:"
    print $ sumNonEmpty [1,2,3,4,5]

    -- HC13T8: Qualified import example
    putStrLn "\nQualified sort of numbers [5,3,1,4,2]:"
    print $ qualifiedSort [5,3,1,4,2]

    -- HC13T9: Renaming module namespace example
    let exampleMap = mapExample ["apple","banana","cherry"]
    putStrLn "\nRenamed module namespace example (Map of fruits):"
    print exampleMap

    -- HC13T10: Multi-module main demonstration
    putStrLn "\nMulti-module demonstration:"
    let finalFiles = sortFilteredFiles ".hs" files
    putStrLn "Sorted '.hs' files in directory:"
    print finalFiles
