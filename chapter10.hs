module Main where

import Data.List (intercalate)

--------------------------------------------------------------------------------
-- HC10T1: ShowSimple Type Class
--------------------------------------------------------------------------------

data PaymentMethod = Cash | CreditCard | BankTransfer
    deriving (Show)

class ShowSimple a where
    showSimple :: a -> String

instance ShowSimple PaymentMethod where
    showSimple Cash         = "Cash"
    showSimple CreditCard   = "Credit Card"
    showSimple BankTransfer = "Bank Transfer"


--------------------------------------------------------------------------------
-- HC10T2: Summable Type Class
--------------------------------------------------------------------------------

class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = sum


--------------------------------------------------------------------------------
-- HC10T3: Comparable Type Class
--------------------------------------------------------------------------------

data Blockchain = Block Int | Chain Blockchain Blockchain
    deriving (Show)

class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
    compareWith (Block a) (Block b) = compare a b
    compareWith (Block _) (Chain _ _) = LT
    compareWith (Chain _ _) (Block _) = GT
    compareWith (Chain a b) (Chain c d) =
        case compareWith a c of
            EQ -> compareWith b d
            x  -> x


--------------------------------------------------------------------------------
-- HC10T4: Eq Instance for Box
--------------------------------------------------------------------------------

data Box a = Empty | Has a
    deriving (Show)

instance Eq a => Eq (Box a) where
    Empty == Empty = True
    Has a == Has b = a == b
    _ == _         = False


--------------------------------------------------------------------------------
-- HC10T5: ShowDetailed Type Class
--------------------------------------------------------------------------------

data User = User { userId :: Int, userName :: String }
    deriving (Show)

class ShowDetailed a where
    showDetailed :: a -> String

instance ShowDetailed User where
    showDetailed (User uid name) =
        "User Details:\n - ID: " ++ show uid ++ "\n - Name: " ++ name


--------------------------------------------------------------------------------
-- HC10T6: Mutual Recursion Eq for Blockchain
--------------------------------------------------------------------------------

instance Eq Blockchain where
    a == b = not (a /= b)
    a /= b =
        case (a, b) of
            (Block x, Block y) -> x /= y
            (Chain a1 b1, Chain a2 b2) -> a1 /= a2 || b1 /= b2
            _ -> True


--------------------------------------------------------------------------------
-- HC10T7: Convertible Type Class
--------------------------------------------------------------------------------

class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert = showSimple


--------------------------------------------------------------------------------
-- HC10T8: AdvancedEq Subclass of Eq
--------------------------------------------------------------------------------

class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

instance AdvancedEq Int where
    compareEquality a b = a == b


--------------------------------------------------------------------------------
-- HC10T9: MinMax Type Class
--------------------------------------------------------------------------------

class MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound
    maxValue = maxBound


--------------------------------------------------------------------------------
-- HC10T10: Concatenatable Type Class
--------------------------------------------------------------------------------

class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable String where
    concatWith = (++)


--------------------------------------------------------------------------------
-- MAIN SECTION
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Haskell Chapter 10 Practical Tasks ==="

    -- HC10T1
    putStrLn "\nHC10T1 ShowSimple:"
    print (showSimple CreditCard)

    -- HC10T2
    putStrLn "\nHC10T2 Summable:"
    print (sumUp [1,2,3,4,5] :: Int)

    -- HC10T3
    putStrLn "\nHC10T3 Comparable Blockchain:"
    let bc1 = Chain (Block 1) (Block 2)
    let bc2 = Chain (Block 1) (Block 3)
    print (compareWith bc1 bc2)

    -- HC10T4
    putStrLn "\nHC10T4 Eq Box:"
    print (Has 5 == Has 5)

    -- HC10T5
    putStrLn "\nHC10T5 ShowDetailed User:"
    putStrLn (showDetailed (User 1 "Alice"))

    -- HC10T6
    putStrLn "\nHC10T6 Mutual Recursion Eq Blockchain:"
    print (bc1 == bc2)

    -- HC10T7
    putStrLn "\nHC10T7 Convertible PaymentMethod -> String:"
    print (convert Cash :: String)

    -- HC10T8
    putStrLn "\nHC10T8 AdvancedEq Int:"
    print (compareEquality 10 10)

    -- HC10T9
    putStrLn "\nHC10T9 MinMax Int:"
    print (minValue :: Int)
    print (maxValue :: Int)

    -- HC10T10
    putStrLn "\nHC10T10 Concatenatable String:"
    print (concatWith "Hello " "World")
