{-# LANGUAGE BlockArguments #-}
import Text.XHtml (base)
import Graphics.Win32 (bST_CHECKED)
-- Activity 3: base 5 function 
base5 :: Int -> [Int]
base5 n
    | n < 5     = [n]
    | otherwise = base5 (n `div` 5) ++ [n `mod` 5]

-- Activity 4: base 8 function 
base8 :: Int -> [Int]
base8 n
    | n < 8     = [n]
    | otherwise = base8 (n `div` 8) ++ [n `mod` 8]

-- Activity 5 : base 16 function 
base16 :: Int -> String
base16 n 
    | n < 16    = [toHexDigit n]
    | otherwise = base16 (n `div` 16) ++ [toHexDigit (n `mod` 16)]

toHexDigit :: Int -> Char
toHexDigit x
    | x >= 0 && x <= 9  = toEnum (fromEnum '0' + x)
    | x >= 10 && x <= 15 = toEnum (fromEnum 'A' + x - 10)
    | otherwise = error "Invalid digit"

-- Activity 6: generator 
generator :: [Int]
generator = [512..624]

-- Activity 7/8: selector 
selector :: Int -> Bool
selector n = equalset (base5 n) (base8 n)

equalset :: [Int] -> [Int] -> Bool
equalset as bs
    = isort as == isort bs

-- Insertion sort
insert :: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs)
    | e <= x    = e : x : xs
    | otherwise = x : insert e xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- Activity 9: main function
main :: IO ()
main = print (head(filter selector generator))