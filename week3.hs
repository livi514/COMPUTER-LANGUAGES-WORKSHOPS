import qualified Control.Applicative as constraints
-- Jack told Gill “I have found three equally-spaced prime numbers 29, 41, and 53.
-- The difference between the first and second is the same as the difference between the
-- second and third, and there are no repeated digits in the six digits of my primes”.
-- Gill told Jack she had also found three equally-spaced primes, each having three
-- digits and with no repeated digits in the nine digits of her primes. She said, “If I
-- told you that the three-digit sum of each of my primes is an odd number then you
-- should be able to find them”.
-- In ascending order, what are Gill’s three primes?
-- The solution given by the newspaper was 157, 283 and 409.

-- This teaser may be seen as a search for three numbers, A, B, and C, satisfying a number
-- of constraints.

-- Activity 1
-- What constraints are there on the form of A, B, and C?
-- These are constraints on the DIGITS/LAYOUT not directly on the numbers themselves.
-- 1. Let's say A's digits are [X3, X2, X1]
-- X3 must be between 1 and 9 inclusive
-- This is because A must be a 3-digit number (trailing 0s don't count)
-- However X2 and X1 can take any values between 0 and 9 inclusive
-- 2. The digits of B, [Y3, Y2, Y1] will follow the same rules, 
-- as B must also be a 3-digit number
-- 3. The same logic applies to C's digits [Z3, Z2, and Z1]
-- 4. The digits X3, X2, X1, Y3, Y2, Y1, Z3, Z2, and Z1 must all be distinct.

-- Activity 2
-- What constraints are there on the values of A, B, and C?
-- 1. A, B, and C must all be prime.
-- 2. A, B, and C must be equally-spaced, so B-A = C-B.
-- 3. A, B, and C must be in ascending order, so A < B and B < C.

-- Activity 3
-- Define a number function that converts a list of digits to a number, 
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

-- Activity 4
-- Define a prime function that takes an integer and returns True if that integer is prime 
-- (divisible by only one and itself)
-- so that prime 43 returns True.
prime :: Int -> Bool
prime n
    -- the smallest prime number is 2
    | n < 2 = False
    -- 2 is prime as its factors are 1 and 2
    | n == 2 = True
    -- 2 is the only even number that is prime 
    -- but we already handled the case if the number is 2 (see above)
    | even n = False
    | otherwise = not (factorisable n 3) -- we've already handled the case if n is 2, so we start at 3  
    where
        factorisable n f
            | n < f * f = False
            | otherwise = mod n f == 0
                -- we already checked even numbers, so we can skip them 
                -- the only even number that's prime is 2, however we already checked whether the number is 2
                || factorisable n (f+2) 

-- Activity 5
-- The first part of a program for this Teaser could be a generator, which is used to construct
-- a list of items that might provide solutions to the problem. That is, a list of lists of lists
-- of three digits.
generator :: [[[Int]]]
generator = [ [[a,b,c],[d,e,f],[g,h,i]]
            | a <- [1..9]
            , b <- [0..9], b `notElem` [a]
            , c <- [0..9], c `notElem` [a,b]
            , d <- [1..9], d `notElem` [a,b,c]
            , e <- [0..9], e `notElem` [a,b,c,d]
            , f <- [0..9], f `notElem` [a,b,c,d,e]
            , g <- [1..9], g `notElem` [a,b,c,d,e,f]
            , h <- [0..9], h `notElem` [a,b,c,d,e,f,g]
            , i <- [0..9], i `notElem` [a,b,c,d,e,f,g,h]
            ]


-- Activity 6
-- The second part of a program for this exercise could be a selector, which is used to filter
-- items that do provide solutions to the problem.
selector :: [[Int]] -> Bool
selector [as,bs,cs]
    = prime a && prime b && prime c
    && b - a == c - b
    && a < b && b < c
    && odd (sum as) && odd (sum bs) && odd (sum cs)
    where 
    a = number as
    b = number bs
    c = number cs

-- Activity 7
main :: IO()
main = print (head (filter selector generator))