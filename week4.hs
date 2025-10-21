-- Teaser:
--My son, at a loose end after A-levels, asked me for a mental challenge, As we’d been
--discussing palindromes, I suggested he try to find an arrangement of the digits 1 to 9,
-- with the multiplication symbol “x” to give a palindrome as the answer, and he
-- came up with 29678x1453 = 43122134. I said “Now try to find the smallest such
-- palindromic product starting in 4, where the last digit of the smallest number is still 3. 
-- He found that smallest product, and, interestingly, if he added the two smaller
-- numbers instead of multiplying them, then added 100, he also go a palindrome.
-- What was the smallest product?
-- The solution given by the newspaper was 40699604 (= 1769548 × 23).

import Data.List

-- Activity 1
-- Write a number function that takes a list of digits and returns the corresponding number,
-- so that number [7,2,6] returns 726.
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
digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = digits d ++ [m]
    where
        (d,m) = n `divMod` 10

-- Activity 3
ends3 :: [Int] -> Bool
ends3 xs = last xs == 3

-- Activity 4
starts4 :: [Int] -> Bool
starts4 xs = head xs == 4

-- Activity 5
palindrome :: [Int] -> Bool
palindrome xs = xs == reverse xs

-- Activity 6 
splits :: [Int] -> [([Int],[Int])]
splits xs = [splitAt n xs | n <- [0..length xs]]

-- Activity 7
generator :: [([Int],[Int])]
generator = concat [splits ps | ps <- permutations [1..9]]

-- Activity 8
selector :: ([Int],[Int]) -> Bool
selector (as,bs)
    = ends3 ms && starts4 ds && palindrome ds && palindrome es
        where 
        a = number as
        b = number bs
        ms = digits (min a b)
        ds = digits (a * b)
        es = digits (a+b+100)

-- Activity 9
main :: IO ()
main 
    = print(filter selector generator)
