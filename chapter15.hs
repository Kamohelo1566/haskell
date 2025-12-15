{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.IO
import Control.Exception
import Data.Typeable
import Text.Read (readMaybe)

-- -----------------------
-- HC15T3: Custom Exception for Traffic Light Errors
-- -----------------------
data TrafficLightError = InvalidTrafficLight String deriving (Show, Typeable)
instance Exception TrafficLightError

-- -----------------------
-- HC15T2 & HC15T4: Self-Driving AI Car System
-- -----------------------
reactToTrafficLight :: String -> IO ()
reactToTrafficLight color = handle handler $ case color of
    "green"  -> putStrLn "Car is moving..."
    "yellow" -> putStrLn "Car is slowing down..."
    "red"    -> putStrLn "Car stopped!"
    _        -> throwIO $ InvalidTrafficLight color
  where
    handler :: TrafficLightError -> IO ()
    handler (InvalidTrafficLight msg) = putStrLn $ "Traffic light error: " ++ msg

-- -----------------------
-- HC15T5: Safe Division Using Maybe
-- -----------------------
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

-- -----------------------
-- HC15T6: Safe Input Parsing with readMaybe
-- -----------------------
safeReadInt :: String -> Maybe Int
safeReadInt = readMaybe

-- -----------------------
-- HC15T7: Velocity Calculation with Optionals and Parsing Handling
-- -----------------------
calcVelocity :: String -> String -> IO ()
calcVelocity distStr timeStr =
    case (safeReadInt distStr, safeReadInt timeStr) of
        (Just d, Just t) ->
            case safeDiv (fromIntegral d) (fromIntegral t) of
                Just v -> putStrLn $ "Velocity: " ++ show v ++ " units/s"
                Nothing -> putStrLn "Error: Division by zero!"
        _ -> putStrLn "Error: Invalid input!"

-- -----------------------
-- HC15T8: Division with Either for Detailed Errors
-- -----------------------
safeDivEither :: Double -> Double -> Either String Double
safeDivEither _ 0 = Left "Error: Division by zero"
safeDivEither x y = Right (x / y)

-- -----------------------
-- HC15T1 & HC15T9: File reading with exceptions
-- -----------------------
readFileSafe :: FilePath -> IO ()
readFileSafe path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left ex  -> putStrLn $ "File error: " ++ show ex
        Right content -> putStrLn $ "File content:\n" ++ content

-- -----------------------
-- HC15T10: Hybrid Error Handling with Either and IO
-- -----------------------
calcVelocityHybrid :: String -> String -> IO ()
calcVelocityHybrid distStr timeStr =
    case (readMaybe distStr :: Maybe Double, readMaybe timeStr :: Maybe Double) of
        (Just d, Just t) ->
            case safeDivEither d t of
                Right v -> putStrLn $ "Hybrid Velocity: " ++ show v ++ " units/s"
                Left msg -> putStrLn msg
        _ -> putStrLn "Hybrid Error: Invalid input!"

-- -----------------------
-- Main Function: Demonstrates all tasks
-- -----------------------
main :: IO ()
main = do
    putStrLn "=== Haskell Chapter 15: Exception and Error Handling ===\n"

    -- HC15T1 & HC15T9: Read file safely
    putStrLn "HC15T1 & HC15T9: Reading 'testfile.txt'"
    readFileSafe "testfile.txt"

    -- HC15T2 & HC15T3 & HC15T4: Traffic light reactions
    putStrLn "\nHC15T2-HC15T4: Traffic light reactions"
    mapM_ reactToTrafficLight ["green","yellow","red","blue"]

    -- HC15T5: Safe division with Maybe
    putStrLn "\nHC15T5: Safe division using Maybe"
    print $ safeDiv 10 2
    print $ safeDiv 10 0

    -- HC15T6: Safe input parsing
    putStrLn "\nHC15T6: Safe input parsing"
    print $ safeReadInt "123"
    print $ safeReadInt "abc"

    -- HC15T7: Velocity calculation with optional values
    putStrLn "\nHC15T7: Velocity calculation with optionals"
    calcVelocity "100" "20"
    calcVelocity "100" "0"
    calcVelocity "one hundred" "20"

    -- HC15T8: Division with Either for detailed errors
    putStrLn "\nHC15T8: Division using Either"
    print $ safeDivEither 10 2
    print $ safeDivEither 10 0

    -- HC15T10: Hybrid error handling
    putStrLn "\nHC15T10: Hybrid velocity calculation"
    calcVelocityHybrid "150" "30"
    calcVelocityHybrid "150" "0"
    calcVelocityHybrid "abc" "10"
