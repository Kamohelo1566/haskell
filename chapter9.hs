module Main where

------------------------------------------
-- HC9T1: Parametric Type Synonym
------------------------------------------
type Entity a = (String, a) 
-- Example: ("User Address", 123) or ("Shop Address", "Cape Town")


------------------------------------------
-- HC9T2: Parametric Data Type Box a
------------------------------------------
data Box a = Empty | Has a
    deriving Show


------------------------------------------
-- HC9T3: addN Function (only for numbers)
------------------------------------------
addN :: Num a => a -> Box a -> Box a
addN _ Empty     = Empty
addN n (Has x)   = Has (x + n)


------------------------------------------
-- HC9T4: extract Function
------------------------------------------
extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x


------------------------------------------
-- HC9T5: Parametric Shape with Record Syntax
------------------------------------------
data Shape a
    = Circle
        { radius :: Float
        , color  :: a
        }
    | Rectangle
        { width  :: Float
        , height :: Float
        , color  :: a
        }
    deriving Show

circle1 :: Shape String
circle1 = Circle 5 "Red"

rect1 :: Shape String
rect1 = Rectangle 10 4 "Blue"


------------------------------------------
-- HC9T6: Recursive Tweet Data Type
------------------------------------------
data Tweet = Tweet
    { content  :: String
    , likes    :: Int
    , comments :: [Tweet]
    } deriving Show

tweet1 :: Tweet
tweet1 = Tweet "Hello!" 5 
    [ Tweet "Nice!" 2 []
    , Tweet "Cool post" 3 
        [ Tweet "Reply to cool post" 1 [] ]
    ]


------------------------------------------
-- HC9T7: Engagement Function
------------------------------------------
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)


------------------------------------------
-- HC9T8: Recursive Sequence Data Type
------------------------------------------
data Sequence a
    = End
    | Node a (Sequence a)
    deriving Show

seq1 :: Sequence Int
seq1 = Node 1 (Node 2 (Node 3 End))


------------------------------------------
-- HC9T9: elemSeq Function
------------------------------------------
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq v (Node x xs)
    | v == x    = True
    | otherwise = elemSeq v xs


------------------------------------------
-- HC9T10: Binary Search Tree Type
------------------------------------------
data BST a
    = EmptyTree
    | NodeBST a (BST a) (BST a)
    deriving Show


------------------------------------------
-- MAIN
------------------------------------------
main :: IO ()
main = do
    putStrLn "=== HC9T1: Entity ==="
    print (("Address1", 100) :: Entity Int)
    print (("Address2", "Shop") :: Entity String)

    putStrLn "\n=== HC9T2 & HC9T3: Box and addN ==="
    print (addN 5 (Has 10))   -- Has 15
    print (addN 3 Empty)      -- Empty

    putStrLn "\n=== HC9T4: extract ==="
    print (extract 0 (Has 7))   -- 7
    print (extract 0 Empty)     -- 0

    putStrLn "\n=== HC9T5: Shapes ==="
    print circle1
    print rect1

    putStrLn "\n=== HC9T6 & HC9T7: Tweet Engagement ==="
    print (engagement tweet1)   -- should sum all likes

    putStrLn "\n=== HC9T8 & HC9T9: Sequence ==="
    print seq1
    print (elemSeq 2 seq1)
    print (elemSeq 5 seq1)

    putStrLn "\n=== HC9T10: BST Type Created ==="
    print (NodeBST 10 EmptyTree EmptyTree)
