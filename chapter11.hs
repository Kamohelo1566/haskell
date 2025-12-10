module Main where

import Data.Char (toUpper)

---------------------------------------------------------------
-- HC11T1: Greet the User
---------------------------------------------------------------
task1 :: IO ()
task1 = do
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "!"

---------------------------------------------------------------
-- HC11T2: Count Characters in a Line
---------------------------------------------------------------
task2 :: IO ()
task2 = do
    putStrLn "Enter a line of text:"
    line <- getLine
    putStrLn $ "Number of characters: " ++ show (length line)

---------------------------------------------------------------
-- HC11T3: Double a Number
---------------------------------------------------------------
task3 :: IO ()
task3 = do
    putStrLn "Enter a number:"
    input <- getLine
    let num = read input :: Int
    putStrLn $ "Double: " ++ show (num * 2)

---------------------------------------------------------------
-- HC11T4: Concatenate Two Lines
---------------------------------------------------------------
task4 :: IO ()
task4 = do
    putStrLn "Enter first line:"
    l1 <- getLine
    putStrLn "Enter second line:"
    l2 <- getLine
    putStrLn $ "Concatenated: " ++ (l1 ++ l2)

---------------------------------------------------------------
-- HC11T5: Repeat Until \"quit\"
---------------------------------------------------------------
task5 :: IO ()
task5 = do
    putStrLn "Type something (type 'quit' to stop):"
    loop
  where
    loop = do
        input <- getLine
        if input == "quit"
            then putStrLn "Goodbye!"
            else do
                putStrLn $ "You typed: " ++ input
                loop

---------------------------------------------------------------
-- HC11T6: Uppercase Converter
---------------------------------------------------------------
task6 :: IO ()
task6 = do
    putStrLn "Enter text to convert to uppercase:"
    text <- getLine
    putStrLn $ map toUpper text

---------------------------------------------------------------
-- HC11T7: User Options
---------------------------------------------------------------
task7 :: IO ()
task7 = do
    putStrLn "Choose an option:"
    putStrLn "1. Say Hello"
    putStrLn "2. Show Current Year (static example)"
    putStrLn "3. Exit"
    choice <- getLine
    case choice of
        "1" -> putStrLn "Hello there!"
        "2" -> putStrLn "The year is 2025."
        "3" -> putStrLn "Exiting..."
        _   -> putStrLn "Invalid choice!"

---------------------------------------------------------------
-- HC11T8: Even or Odd Checker
---------------------------------------------------------------
task8 :: IO ()
task8 = do
    putStrLn "Enter a number:"
    input <- getLine
    let n = read input :: Int
    putStrLn $ if even n then "Even" else "Odd"

---------------------------------------------------------------
-- HC11T9: Sum Two Numbers
---------------------------------------------------------------
task9 :: IO ()
task9 = do
    putStrLn "Enter first number:"
    a <- readLn :: IO Int
    putStrLn "Enter second number:"
    b <- readLn :: IO Int
    putStrLn $ "Sum: " ++ show (a + b)

---------------------------------------------------------------
-- HC11T10: Reverse User Input
---------------------------------------------------------------
task10 :: IO ()
task10 = do
    putStrLn "Enter text to reverse:"
    txt <- getLine
    putStrLn $ reverse txt

---------------------------------------------------------------
-- MAIN MENU (run any task)
---------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== Haskell Chapter 11 I/O Tasks ==="
    putStrLn "Select a task to run (1–10):"
    putStrLn "1. Greet the User"
    putStrLn "2. Count Characters"
    putStrLn "3. Double a Number"
    putStrLn "4. Concatenate Lines"
    putStrLn "5. Repeat Until Quit"
    putStrLn "6. Uppercase Converter"
    putStrLn "7. User Options"
    putStrLn "8. Even or Odd"
    putStrLn "9. Sum Two Numbers"
    putStrLn "10. Reverse Input"
    putStrLn "Enter your choice:"

    choice <- getLine
    case choice of
        "1"  -> task1
        "2"  -> task2
        "3"  -> task3
        "4"  -> task4
        "5"  -> task5
        "6"  -> task6
        "7"  -> task7
        "8"  -> task8
        "9"  -> task9
        "10" -> task10
        _    -> putStrLn "Invalid choice!"
