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
-- Write an ends3 function that takes a list of integers and returns True if the list ends with three,
-- and False otherwise, so that ends3 [7,6,5,4,3] returns True.
ends3 :: [Int] -> Bool
-- last is used to get the last element of a list
ends3 xs = last xs == 3

-- Activity 4
-- Write a starts4 function that takes a list of integers and returns True if the list starts
-- with four, and False otherwise, so that starts4 [4,5,6,7,8] returns True.
starts4 :: [Int] -> Bool
-- head is used to get the first element of a list
starts4 xs = head xs == 4

-- Activity 5
-- Write a palindrome function that takes a list of integers and returns True if the list is a
-- palindrome, and False otherwise, so that palindrome [4,5,6,7,6,5,4] returns True.
palindrome :: [Int] -> Bool
-- here, I simply compared a list with its inverse (xs == reverse xs)
palindrome xs = xs == reverse xs

-- Activity 6 
-- Write a splits function that takes a list and returns a list of all splits of that list as pairs,
-- so that splits [1,2,3] ⇝ [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
splits :: [Int] -> [([Int],[Int])]
-- iterating through different positions to split the list at, starting from 0 
-- and going to the length of xs
splits xs = [splitAt n xs | n <- [0..length xs]]

-- Activity 7
-- The first part of a program for this Teaser could be a generator, which is used to construct
-- a list of items that might provide solutions to the problem. That is, pairs of lists of integers
-- from the permuatations of [1..9] that starts
-- [([],[1,2,3,4,5,6,7,8,9])
-- ,([1],[2,3,4,5,6,7,8,9])
-- ,([1,2],[3,4,5,6,7,8,9])
-- ,([1,2,3],[4,5,6,7,8,9])
-- ,([1,2,3,4],[5,6,7,8,9])
-- ,([1,2,3,4,5],[6,7,8,9])
-- ...
--]

generator :: [([Int],[Int])]
-- permutations is a built-in function that returns all the possible permutations of the argument
generator = concat [splits ps | ps <- permutations [1..9]]

-- Activity 8
-- The second part of a program for this exercise could be a selector, which is used to filter
-- items that do provide solutions to the problem.
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
-- The final part of our program for this teaser is a main function that puts the generator
-- and selector together, filtering the list from the generator with the selector.
main :: IO ()
main 
    = print(filter selector generator)
-- The result of this program is ([2,3],[1,7,6,9,5,4,8]) from which the solution, 40699604
-- given by the newspaper may be calculated directly.