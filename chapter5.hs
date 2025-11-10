-- HC5T1: Using applyTwice
-- Define a function that applies a given function three times to an integer.
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))


-- HC5T2: Filtering Odd Numbers (1 to 30)
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]


-- HC5T3: Checking for Uppercase Letters
-- Check if any word starts with an uppercase letter
hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\w -> not (null w) && head w `elem` ['A'..'Z'])


-- HC5T4: Using Lambda Functions
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10


-- HC5T5: Partial Application
multiplyByFive :: Int -> Int
multiplyByFive = (*5)


-- HC5T6: Function Composition
-- Squares the numbers, then filters to keep even ones
squareEvenNumbers :: [Int] -> [Int]
squareEvenNumbers = filter even . map (^2)


-- HC5T7: The $ Operator
-- Rewriting using $
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]


-- HC5T8: Point-Free Style
addFive :: Int -> Int
addFive = (+5)


-- HC5T9: Higher-Order Function to Transform a List
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)


-- HC5T10: Combining Higher-Order Functions
-- Check if any squared value in a list is greater than 50
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2)


-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
    putStrLn "=== HC5T1 - Apply Function Thrice ==="
    print (applyThrice (+2) 5)      -- 5 + 2 + 2 + 2 = 11
    print (applyThrice (*2) 1)      -- 1*2*2*2 = 8

    putStrLn "\n=== HC5T2 - Filter Odd Numbers (1..30) ==="
    print oddNumbers

    putStrLn "\n=== HC5T3 - Check for Uppercase Words ==="
    print (hasUppercaseWord ["apple", "Banana", "cat"])
    print (hasUppercaseWord ["dog", "egg", "fish"])

    putStrLn "\n=== HC5T4 - Lambda Function ==="
    print (biggerThan10 5)
    print (biggerThan10 15)

    putStrLn "\n=== HC5T5 - Partial Application (Multiply by 5) ==="
    print (multiplyByFive 6)
    print (multiplyByFive 12)

    putStrLn "\n=== HC5T6 - Function Composition (Square Evens) ==="
    print (squareEvenNumbers [1..10])

    putStrLn "\n=== HC5T7 - Using $ Operator ==="
    print result

    putStrLn "\n=== HC5T8 - Point-Free Style (Add 5) ==="
    print (addFive 10)

    putStrLn "\n=== HC5T9 - Transform List (Apply Twice) ==="
    print (transformList (+1) [1,2,3])
    print (transformList (*2) [1,2,3])

    putStrLn "\n=== HC5T10 - Combine Higher-Order Functions ==="
    print (anySquareGreaterThan50 [1,5,8])
    print (anySquareGreaterThan50 [2,3,6])
