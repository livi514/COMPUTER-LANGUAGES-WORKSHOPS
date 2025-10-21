import qualified Control.Applicative as problem
-- Teaser:
--Each year when I get my new diary, I like to look at the dates to see if they have
--any interesting properties. When I looked at this year’s diary, I was pleased to find
--two dates that are “square” in the following way. If the date is expressed with one
--or two digits for the day (ie, no leading zero is allowed), followed by two digits for
--the month and then the year in full, then 1.09.2025 is a square date, since 1092025
--is the square of 1045.
--The only other square date this year is 27.09.2025, since 27092025 is the square of 5205.
--Using the same method of expressing the date, what is the first square date after 2025?
--The solution given by the newspaper was 1.01.2036.

-- WARMUP

-- Activity 1
-- Write a number function that takes a list of digits and returns the corresponding number,
-- so that number [7,2,6] returns 726.
-- taking a list of digits and returning the corresponding number
number :: [Int] -> Int
-- reverse the list to process from least significant digit
-- totalize function to accumulate the number
-- we apply totalize to the reversed list, since totalize processes from least significant digit
number xs = totalize (reverse xs)
    where
    -- helper function to compute the number
    -- Explanation of totalize:
    -- For each digit x in the list, it adds x multiplied by 10 raised to the power of its position index.
    -- This effectively constructs the number by placing each digit in its correct place value.
    totalize (x :xs) = x + 10 * totalize xs
    -- if the list is empty, return 0
    totalize [] = 0

-- Activity 2
-- Write a digits function that takes an integer and returns a list of its digits, 
-- so that digits 726 returns [7,2,6].
-- taking an integer and returning a list of its digits
digits :: Int -> [Int]
digits n
    -- for a single digit number, the list will only contain a single element - that digit
    | n < 10 = [n]
    -- for numbers with more than one digit, we recursively divide the number by 10
    | otherwise = digits d ++ [m]
    where
        -- divMod divides n by 10 and returns a tuple (d, m)
        -- d is the quotient (all digits except the last)
        -- m is the remainder (the last digit)
        (d,m) = n `divMod` 10

-- Activity 3
-- Write a perfect function that takes an integer n and returns True if n is a perfect square, and False otherwise,
-- so that perfect 1024 returns True.
-- Other examples:
-- perfect 1023 returns False
-- perfect 0 returns True
-- perfect 1 returns True
-- perfect 16 returns True
-- perfect 1000 returns False
-- perfect 10000 returns True

-- taking an integer n and returning True if n is a perfect square, False otherwise
perfect :: Int -> Bool
perfect n 
    -- check if the square of the integer part of the square root of n equals n
    = r * r == n 
    where 
        -- floor - converts a floating-point number to the largest integer less than or equal to it
        -- floor is basically rounding down 
        -- fromIntegral converts an Int to a Float for the sqrt function
        r = floor (sqrt (fromIntegral n))

-- Activity 4
-- Write a pad function that takes an integer length, an item, and a list of items, 
-- and pads out the list to the length, so that pad 8 0 [1,2,3] returns [0,0,0,0,0,1,2,3].

-- taking an integer length n, an item x, and a list xs, and padding the list to the length n
-- returning a list of digits (the padded representation of the number, each element is one digit)
pad :: Int -> [Int] -> [Int]
pad n xs
    -- if the length of the list is less than n, we recursively call pad
    | length xs < n = pad n (0:xs)
    -- otherwise, we return the list as is
    | otherwise = xs

-- Activity 4 - polymorphic version 
-- a generic version of pad that works for any type of item, not just Int
pad2 :: Int -> a -> [a] -> [a]
pad2 n x xs
    | length xs < n = pad2 n x (x:xs)
    | otherwise = xs

-- Activity 5
-- Noting that a leap year is a year that is divisible by 4, except for years ending in 00,
-- which must be divisible by 400, write a leap function that takes an integer year,
-- and returns True if the integer is a leap year, and False otherwise, 
-- so that leap 2024 returns True.
leap :: Int -> Bool
leap year 
    -- check the conditions for a leap year
    -- any year divisible by 400 will be a leap year
    -- in the description for activity 5, 
    -- it is mentioned that years ending in 00 must be divisible by 400 to be leap years
    -- I decided to implement it this way, because any year divisible by 400 will be a leap year,
    -- then I covered the case for years divisible by 100 but not by 400 below.
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    -- any year divisible by 4 that wasn't excluded by the previous condition will be a leap year
    | year `mod` 4 == 0 = True
    | otherwise = False

-- Activity 6
-- Write a days function that takes an integer month and an integer year, 
-- and returns the number of days in that month of that year, 
-- so that days 2 2024 returns 29.
days :: Int -> Int -> Int
days month year
    -- check for February and whether it's a leap year
    | month == 2 && leap year = 29
    | month == 2 = 28
    -- check for months with 30 days
    | month `elem` [4,6,9,11] = 30
    | otherwise = 31

--  Activity 7
-- The first part of a program for this Teaser could be a generator, 
-- which is used to construct a list of items that might provide solutions to the problem.
-- That is, a list of triples (DY, MO, YR) of days, months and years that represent valid dates.

-- date is expressed with 1 or 2 digits for the day (no leading zeros)
-- two digits for the month (leading zero if needed)
-- and 4 digits for the year
-- constraints: year > 2025, no upper bound given 
-- upper bound can be edited if needed, I set it to 2100 for now for efficiency
generator :: [(Int,Int,Int)]
generator 
    = [
        (dy, mo, yr) | yr <- [2026..2100],
                        mo <- [1..12],
                        dy <- [1..days mo yr]
    ]

-- Activity 8 
-- The second part of this program could be a selector,
-- which is used to filter items that do provide solutions to the problem.

-- If the date is expressed with one
--or two digits for the day (ie, no leading zero is allowed), followed by two digits for
--the month and then the year in full, then 1.09.2025 is a square date, since 1092025
--is the square of 1045.
selector :: (Int,Int,Int) -> Bool
selector (d,m,y)
    = perfect n
        where 
        n = number (digits d ++ pad2 2 0 (digits m) ++ digits y)

-- Activity 9
-- The final part of our program for this teaser is a main function, 
-- that puts the generator and selector together, 
-- filtering the list from the generator with the selector.
main :: IO ()
main 
    = print (head (filter selector generator))