module Main where

------------------------------------------
-- HC8T1: Type Synonyms and Basic Function
------------------------------------------
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount =
    "From: " ++ from ++ ", To: " ++ to ++ ", Amount: " ++ show amount


------------------------------------------
-- HC8T2: New Types and Data Constructors
------------------------------------------
data PaymentMethod = Cash | Card | Cryptocurrency
    deriving Show

data Person2 = Person2
    { pName :: String
    , pAddress :: (String, Int)
    , pMethod :: PaymentMethod
    } deriving Show

bob :: Person2
bob = Person2 "Bob" ("Main Street", 101) Cash


------------------------------------------
-- HC8T3: Algebraic Data Types and Functions
------------------------------------------
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

circleArea :: Float
circleArea = area (Circle 5)

rectArea :: Float
rectArea = area (Rectangle 10 5)


------------------------------------------
-- HC8T4: Record Syntax Employee
------------------------------------------
data Employee = Employee
    { name :: String
    , experienceInYears :: Float
    } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5


------------------------------------------
-- HC8T5: Person with Record Syntax
------------------------------------------
data Person = Person
    { personName :: String
    , age :: Int
    , isEmployed :: Bool
    } deriving Show

person1 :: Person
person1 = Person "Alice" 30 True

person2 :: Person
person2 = Person "Tom" 22 False


------------------------------------------
-- HC8T6: Record Syntax for Shape Variants
------------------------------------------
data Shape2
    = Circle2
        { center :: (Float, Float)
        , color :: String
        , radius :: Float
        }
    | Rectangle2
        { width :: Float
        , height :: Float
        , color :: String
        } deriving Show

circleInstance :: Shape2
circleInstance = Circle2 (0, 0) "Red" 5

rectInstance :: Shape2
rectInstance = Rectangle2 10 5 "Blue"


------------------------------------------
-- HC8T7: Animal Data Type and Function
------------------------------------------
data Animal = Dog String | Cat String

describeAnimal :: Animal -> String
describeAnimal (Dog n) = "A dog named " ++ n
describeAnimal (Cat n) = "A cat named " ++ n

dog1 :: Animal
dog1 = Dog "Rex"

cat1 :: Animal
cat1 = Cat "Mittens"


------------------------------------------
-- HC8T8: Type Synonyms and Greeting
------------------------------------------
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello " ++ n ++ ", you are " ++ show a ++ " years old!"


------------------------------------------
-- HC8T9: Record Type and Transaction Function
------------------------------------------
data Transaction = Transaction
    { from :: Address
    , to :: Address
    , amount :: Value
    , transactionId :: String
    } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t amt =
    let tx = Transaction f t amt "TX12345"
    in transactionId tx


------------------------------------------
-- HC8T10: Book with deriving Show
------------------------------------------
data Book = Book
    { title :: String
    , author :: String
    , year :: Int
    } deriving Show

book1 :: Book
book1 = Book "The Haskell Road" "John Doe" 2020


------------------------------------------
-- MAIN
------------------------------------------
main :: IO ()
main = do
    putStrLn "=== HC8T1 ==="
    print (generateTx "A1" "B1" 100)

    putStrLn "\n=== HC8T2 ==="
    print bob

    putStrLn "\n=== HC8T3 ==="
    putStrLn ("Area of Circle (r=5): " ++ show circleArea)
    putStrLn ("Area of Rectangle (10x5): " ++ show rectArea)

    putStrLn "\n=== HC8T4 ==="
    print richard

    putStrLn "\n=== HC8T5 ==="
    print person1
    print person2

    putStrLn "\n=== HC8T6 ==="
    print circleInstance
    print rectInstance

    putStrLn "\n=== HC8T7 ==="
    putStrLn (describeAnimal dog1)
    putStrLn (describeAnimal cat1)

    putStrLn "\n=== HC8T8 ==="
    putStrLn (greet "Kamohelo" 25)

    putStrLn "\n=== HC8T9 ==="
    putStrLn ("Transaction ID: " ++ createTransaction "Alice" "Bob" 999)

    putStrLn "\n=== HC8T10 ==="
    print book1
