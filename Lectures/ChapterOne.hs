module Lectures.ChapterOne where

{-
    1.2 Functional composition
    f :: Y -> Z 
    g :: X -> Y
    f . g :: X -> Z
    -> association is from right to left
    (f . g) x = f (g x)
-}

{-
    1.4 Example: numbers into words
-}
units, teens, tens :: [String]
units = ["zero","one","two","three","four","five",
    "six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourteen",
    "fifteen","sixteen","seventeen","eighteen","nineteen"]
tens = ["twenty","thirty","forty",
    "fifty", "sixty","seventy","eighty","ninety"]

-- 0 <= n < 10
convert1 :: Int -> String
convert1 n = units !! n

-- 0 <= n < 100
convert2 :: Int -> String
convert2 n
    | t == 0           = units !! s
    | t == 1           = teens !! s
    | s == 0           = tens !! (t - 2)
    | otherwise        = tens !! (t - 2) ++ "-" ++ units !! s
    where (t, s) = (n `div` 10, n `mod` 10)

-- 0 <= n < 1000
convert3 :: Int -> String
convert3 n
    | h == 0 = convert2 t
    | n == 0 = units !! h ++ " hundred" 
    | otherwise = units !! h ++ " hundred and " ++ convert2 t
    where (h,t) =(n `div`100, n `mod` 100)

-- 0 <= n < 1,000,000
convert6 :: Int -> String
convert6 n
    | m == 0 = convert3 h
    | h == 0 = convert3 m ++ " thousand"
    | otherwise = convert3 m ++ " thousand and " ++ link h ++ convert3 h
    where (m,h) =(n `div` 1000, n `mod` 1000)

link :: Int -> String
link h 
    | h < 100 = " and " 
    | otherwise = " "

convert :: Int -> String
convert = convert6
