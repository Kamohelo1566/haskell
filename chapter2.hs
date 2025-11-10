-- HC2T1 - Task 1: Checking Types in GHCi
-- Expected types before checking:
-- 42            :: Int
-- 3.14          :: Floating a => a (default Double)
-- "Haskell"     :: [Char] (String)
-- 'Z'           :: Char
-- True && False :: Bool


-- HC2T2 - Task 2: Function Type Signatures and Implementation
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2


-- HC2T3 - Task 3: Immutable Variables
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- Note: Variables in Haskell are immutable.
-- If you try to reassign, e.g., myAge = 30 later, it will cause a compile error.


-- HC2T4 - Task 4: Converting Between Infix and Prefix Notations
-- Infix → Prefix examples:
-- 5 + 3        → (+) 5 3
-- 10 * 4       → (*) 10 4
-- True && False → (&&) True False

-- Prefix → Infix examples:
-- (+) 7 2     → 7 + 2
-- (*) 6 5     → 6 * 5
-- (&&) True False → True && False


-- HC2T5 - Task 5: Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r ^ 2

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)


-- HC2T6 - Task 6: Understanding Int vs Integer
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127
-- Note: Try evaluating `2^64 :: Int` in GHCi — it will overflow!
-- But `2^64 :: Integer` will give the correct large value.


-- HC2T7 - Task 7: Boolean Expressions
bool1 :: Bool
bool1 = True && (5 < 10)  -- True using &&

bool2 :: Bool
bool2 = False || (3 > 5)  -- False using ||

bool3 :: Bool
bool3 = not False         -- True using not

bool4 :: Bool
bool4 = 7 == 8            -- comparison that returns False


-- MAIN FUNCTION FOR TESTING
main :: IO ()
main = do
    putStrLn "=== HC2T1 - Expected Types ==="
    putStrLn "42 :: Int"
    putStrLn "3.14 :: Floating a => a"
    putStrLn "\"Haskell\" :: String"
    putStrLn "'Z' :: Char"
    putStrLn "True && False :: Bool"

    putStrLn "\n=== HC2T2 - Function Tests ==="
    print (add 5 3)
    print (isEven 4)
    print (isEven 7)
    print (concatStrings "Hello, " "World!")

    putStrLn "\n=== HC2T3 - Immutable Variables ==="
    print myAge
    print piValue
    print greeting
    print isHaskellFun

    putStrLn "\n=== HC2T4 - Infix/Prefix Notation ==="
    print ((+) 5 3)        -- prefix version
    print (10 * 4)         -- infix version
    print ((&&) True False)
    print (7 + 2)
    print (True && False)

    putStrLn "\n=== HC2T5 - circleArea & maxOfThree ==="
    print (circleArea 5)
    print (maxOfThree 10 22 15)

    putStrLn "\n=== HC2T6 - Int vs Integer ==="
    print smallNumber
    print bigNumber
    putStrLn "Try evaluating 2^64 :: Int and 2^64 :: Integer in GHCi"

    putStrLn "\n=== HC2T7 - Boolean Expressions ==="
    print bool1
    print bool2
    print bool3
    print bool4
