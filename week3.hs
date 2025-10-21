-- Activity 3
number :: [Int] -> Int
number xs = totalize (reverse xs)
    where
    totalize (x :xs) = x + 10 * totalize xs
    totalize [] = 0

-- Activity 4
prime :: Int -> Bool
prime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = not (factorisable n 3) -- we've already handled the case if n is 2, so we start at 3 -- 
    where
        factorisable n f
            | n < f * f = False
            | otherwise = mod n f == 0
                || factorisable n (f+2) -- we already checked even numbers, so we can skip them --

-- Activity 5
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