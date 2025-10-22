import Data.List (sortBy)

-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- HC1T2 - Task 2: Pure Function Example
circleArea :: Float -> Float
circleArea r = pi * r * r

-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- HC1T4 - Task 4: Composing a Function to Process Player Data
extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = [name | (name, _) <- players]

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [(String, Int)] -> [(String, Int)]
topThree players = take 3 players

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

takeFirstN :: Int -> [Int]
takeFirstN n = take n infiniteNumbers

-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Float -> Float
fToC f = (f - 32) * 5 / 9

-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to demonstrate all tasks
main :: IO ()
main = do
    putStrLn "HC1T1 - Function Composition:"
    print (doubleThenIncrement 5)

    putStrLn "\nHC1T2 - Pure Function Example (Circle Area):"
    print (circleArea 3)

    putStrLn "\nHC1T3 - Greater than 18:"
    print (greaterThan18 20)
    print (greaterThan18 10)

    putStrLn "\nHC1T4 - Get Top Three Players:"
    let players = [("Alice", 90), ("Bob", 75), ("Carol", 95), ("Dave", 80)]
    print (getTopThreePlayers players)

    putStrLn "\nHC1T5 - Laziness (First 5 Numbers):"
    print (takeFirstN 5)

    putStrLn "\nHC1T6 - Add Numbers:"
    print (addNumbers 7 8)

    putStrLn "\nHC1T7 - Fahrenheit to Celsius:"
    print (fToC 98.6)

    putStrLn "\nHC1T8 - Apply Function Twice (increment):"
    print (applyTwice increment 5)
