import Data.List

-- Activity 1
number :: [Int] -> Int
number xs = totalize (reverse xs)
    where
    totalize (x :xs) = x + 10 * totalize xs
    totalize [] = 0

-- Activity 2
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
