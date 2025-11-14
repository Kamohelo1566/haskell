-- Haskell Chapter 7 Practical Tasks: Type Classes and Custom Types

-- HC7T1: Implement an Eq Instance for a Custom Data Type
data Color = Red | Green | Blue
    deriving (Show, Read)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

-- HC7T2: Implement an Ord Instance for a Custom Data Type
instance Ord Color where
    Red   <= _     = True
    Green <= Green = True
    Green <= Blue  = True
    Blue  <= Blue  = True
    _     <= _     = False

-- HC7T3: Function Using Multiple Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Custom Type with Show and Read
data Shape = Circle Double | Rectangle Double Double
    deriving (Read, Eq, Ord) -- Ord added for comparisons

instance Show Shape where
    show (Circle r) = "Circle with radius " ++ show r
    show (Rectangle w h) = "Rectangle with width " ++ show w ++ " and height " ++ show h

-- HC7T5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea s = s * s

-- HC7T6: Using Integral and Floating Type Classes
circleCircumference :: (Floating a) => a -> a
circleCircumference r = 2 * pi * r

-- HC7T7: Bounded and Enum
data Color2 = CRed | CGreen | CBlue
    deriving (Eq, Ord, Show, Enum, Bounded)

nextColor :: Color2 -> Color2
nextColor col = if col == maxBound then minBound else succ col

-- HC7T8: Parse a Value from a String Using Read
parseShape :: String -> Maybe Shape
parseShape s =
    case reads s of
        [(sh, "")] -> Just sh
        _          -> Nothing

-- HC7T9: Type Class with Multiple Instances
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "This is True."
    describe False = "This is False."

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle (" ++ show w ++ " x " ++ show h ++ ")"

-- HC7T10: Function with Multiple Type Class Constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = if x >= y then describe x else describe y

-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
    putStrLn "=== HC7T1/2/3 - Color and compareValues ==="
    print (compareValues Red Green)
    print (compareValues Green Blue)

    putStrLn "\n=== HC7T4 - Shape Show and Read ==="
    print (Circle 5)
    print (Rectangle 3 4)

    putStrLn "\n=== HC7T5 - squareArea ==="
    print (squareArea 5)

    putStrLn "\n=== HC7T6 - circleCircumference ==="
    print (circleCircumference 10)

    putStrLn "\n=== HC7T7 - nextColor ==="
    print (nextColor CRed)
    print (nextColor CBlue)

    putStrLn "\n=== HC7T8 - parseShape ==="
    print (parseShape "Circle 7")
    print (parseShape "Rectangle 2 5")
    print (parseShape "Unknown")

    putStrLn "\n=== HC7T9 - Describable ==="
    putStrLn (describe True)
    putStrLn (describe (Circle 7))

    putStrLn "\n=== HC7T10 - describeAndCompare ==="
    putStrLn (describeAndCompare (Circle 5) (Circle 3))
