-- HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"


-- HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"


-- HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment n
    | n >= 90 && n <= 100 = "Excellent!"
    | n >= 70 && n <= 89  = "Good job!"
    | n >= 50 && n <= 69  = "You passed."
    | n >= 0  && n <= 49  = "Better luck next time."
    | otherwise            = "Invalid grade"


-- HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! Happy 1st!"
specialBirthday 16 = "Sweet sixteen!"
specialBirthday 18 = "Finally an adult!"
specialBirthday 21 = "Cheers to 21!"
specialBirthday _  = "Happy birthday!"


-- HC4T5 - Task 5: Add Catch-All Pattern with Age in Message
specialBirthdayWithAge :: Int -> String
specialBirthdayWithAge 1  = "First birthday! Happy 1st!"
specialBirthdayWithAge 16 = "Sweet sixteen!"
specialBirthdayWithAge 18 = "Finally an adult!"
specialBirthdayWithAge 21 = "Cheers to 21!"
specialBirthdayWithAge age = "Happy " ++ show age ++ "th Birthday!"


-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList [] = "The list is empty."
whatsInsideThisList [_] = "The list has one element."
whatsInsideThisList [_, _] = "The list has two elements."
whatsInsideThisList _ = "The list has many elements."


-- HC4T7 - Task 7: Ignore Elements in a List
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _ = []


-- HC4T8 - Task 8: Extract Values from Tuples
describeTuple :: (String, Int, Char) -> String
describeTuple (name, age, grade) =
    "Name: " ++ name ++ ", Age: " ++ show age ++ ", Grade: " ++ [grade]


-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
    putStrLn "=== HC4T1 - Weather Report ==="
    print (weatherReport "sunny")
    print (weatherReport "rainy")
    print (weatherReport "foggy")

    putStrLn "\n=== HC4T2 - Day Type ==="
    print (dayType "Monday")
    print (dayType "Saturday")
    print (dayType "Holiday")

    putStrLn "\n=== HC4T3 - Grade Comment ==="
    print (gradeComment 95)
    print (gradeComment 75)
    print (gradeComment 30)
    print (gradeComment 120)

    putStrLn "\n=== HC4T4 - Special Birthday ==="
    print (specialBirthday 1)
    print (specialBirthday 16)
    print (specialBirthday 10)

    putStrLn "\n=== HC4T5 - Special Birthday with Age ==="
    print (specialBirthdayWithAge 18)
    print (specialBirthdayWithAge 25)

    putStrLn "\n=== HC4T6 - What's Inside List ==="
    print (whatsInsideThisList ([] :: [Int]))
    print (whatsInsideThisList [1])
    print (whatsInsideThisList [1, 2])
    print (whatsInsideThisList [1, 2, 3, 4])

    putStrLn "\n=== HC4T7 - First and Third Elements ==="
    print (firstAndThird [10, 20, 30, 40])
    print (firstAndThird [5])
    print (firstAndThird [1, 2, 3])

    putStrLn "\n=== HC4T8 - Describe Tuple ==="
    print (describeTuple ("Alice", 20, 'A'))
    print (describeTuple ("Bob", 17, 'B'))
