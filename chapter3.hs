-- HC3T1 - Task 1: Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber n =
    if n > 0 then "Positive"
    else if n < 0 then "Negative"
    else "Zero"


-- HC3T2 - Task 2: Determine the grade based on a score using guards
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"


-- HC3T3 - Task 3: Convert an RGB color to a hex string using let bindings
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let toHex x = let h = "0123456789ABCDEF"
                      a = x `div` 16
                      b' = x `mod` 16
                  in [h !! a, h !! b']
    in "#" ++ toHex r ++ toHex g ++ toHex b


-- HC3T4 - Task 4: Calculate the area of a triangle using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))


-- HC3T5 - Task 5: Determine the type of a triangle using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"


-- HC3T6 - Advanced Task 6: Check leap year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False


-- HC3T7 - Advanced Task 7: Determine the season based on the month using guards
season :: Int -> String
season m
    | m == 12 || m == 1 || m == 2 = "Winter"
    | m == 3 || m == 4 || m == 5 = "Spring"
    | m == 6 || m == 7 || m == 8 = "Summer"
    | m == 9 || m == 10 || m == 11 = "Autumn"
    | otherwise = "Invalid month"


-- HC3T8 - Advanced Task 8: Calculate BMI and return category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25 = "Normal"
    | bmi < 30 = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / (height ^ 2)


-- HC3T9 - Advanced Task 9: Find the maximum of three numbers using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = if a > b then a else b
        maxABC = if maxAB > c then maxAB else c
    in maxABC


-- HC3T10 - Advanced Task 10: Check if a string is a palindrome using recursion and guards
isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1 = True
    | head str == last str = isPalindrome (init (tail str))
    | otherwise = False


-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
    putStrLn "=== HC3T1 - Check Number ==="
    print (checkNumber 5)
    print (checkNumber (-3))
    print (checkNumber 0)

    putStrLn "\n=== HC3T2 - Grade ==="
    print (grade 95)
    print (grade 72)
    print (grade 50)

    putStrLn "\n=== HC3T3 - RGB to Hex ==="
    print (rgbToHex (255, 0, 127))
    print (rgbToHex (0, 255, 64))

    putStrLn "\n=== HC3T4 - Triangle Area ==="
    print (triangleArea 3 4 5)
    print (triangleArea 7 8 9)

    putStrLn "\n=== HC3T5 - Triangle Type ==="
    print (triangleType 3 3 3)
    print (triangleType 5 5 8)
    print (triangleType 6 7 8)

    putStrLn "\n=== HC3T6 - Leap Year ==="
    print (isLeapYear 2000)
    print (isLeapYear 1900)
    print (isLeapYear 2024)

    putStrLn "\n=== HC3T7 - Season ==="
    print (season 3)
    print (season 7)
    print (season 11)

    putStrLn "\n=== HC3T8 - BMI Category ==="
    print (bmiCategory 70 1.75)
    print (bmiCategory 90 1.8)

    putStrLn "\n=== HC3T9 - Max of Three ==="
    print (maxOfThree 10 20 15)
    print (maxOfThree 5 25 10)

    putStrLn "\n=== HC3T10 - Palindrome Check ==="
    print (isPalindrome "racecar")
    print (isPalindrome "haskell")
    print (isPalindrome "madam")
