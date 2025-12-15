-- HC12 Practical Tasks: Haskell Programs and Modules

-- Importing required modules
import System.IO
import Control.Exception
import Data.List (sort)

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> [Integer]
fibonacci n = map fib [0..(n-1)]
  where
    fib 0 = 0
    fib 1 = 1
    fib k = fib (k-1) + fib (k-2)

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

-- HC12T6: Sort a List of Integers
sortList :: [Int] -> [Int]
sortList = sort

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- HC12T9: Read and Print File Content
printFileContent :: FilePath -> IO ()
printFileContent path = catch
  (do contents <- readFile path
      putStrLn "File content:"
      putStrLn contents)
  (\e -> putStrLn $ "Error: " ++ show (e :: IOException))

-- HC12T10: Mathematical Operations Module
module MathOperations (add, multiply) where
add :: Int -> Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

-- Main Function
main :: IO ()
main = do
  -- HC12T1: Welcome message
  putStrLn "Welcome to Haskell Programming!"

  -- HC12T2: Add two numbers
  putStrLn "\nSum of 5 and 10:"
  print $ addTwoNumbers 5 10

  -- HC12T3: Factorial
  putStrLn "\nFactorial of 5:"
  print $ factorial 5

  -- HC12T4: First 10 Fibonacci numbers
  putStrLn "\nFirst 10 Fibonacci numbers:"
  print $ fibonacci 10

  -- HC12T5: Palindrome Checker
  putStrLn "\nEnter a string to check palindrome:"
  str <- getLine
  putStrLn $ if isPalindrome str then "It is a palindrome" else "Not a palindrome"

  -- HC12T6: Sort a list of integers
  putStrLn "\nEnter integers separated by spaces to sort:"
  numsStr <- getLine
  let nums = map read (words numsStr) :: [Int]
  putStrLn "Sorted list:"
  print $ sortList nums

  -- HC12T7: Circle area
  putStrLn "\nArea of circle with radius 5:"
  print $ calculateCircleArea 5

  -- HC12T8: Merge two sorted lists
  let list1 = [1,3,5]
  let list2 = [2,4,6]
  putStrLn "\nMerging [1,3,5] and [2,4,6]:"
  print $ mergeLists list1 list2

  -- HC12T9: Read file content
  putStrLn "\nReading content of 'sample.txt' (if exists):"
  printFileContent "sample.txt"

  -- HC12T10: Using MathOperations module
  putStrLn "\nUsing MathOperations module:"
  putStrLn $ "Add 3 + 4 = " ++ show (MathOperations.add 3 4)
  putStrLn $ "Multiply 3 * 4 = " ++ show (MathOperations.multiply 3 4)
