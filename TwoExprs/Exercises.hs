module TwoExprs.Exercises where

{-
    Exercise A
    Is a half of two plus two equal to two or three?
    It is depends on precedence of "half of" and "plus"
    -  (half of two) plus two
    -  half of (two plus two)
-}


{-
    Exercise B
    - [0, 1)          -> [0, 1]
    - double -3       -> double (-3)
    - double (-3)
    - double double 0 -> double.double 0 or double (double 0)
    - if 1==0 then 2==1 -> need else cause
    - "++" == "+" ++ "+"
    - [[],[[]],[[[]]]]
    - concat ["tea","for",'2'] -> concat ["tea","for","2"]
    - concat ["tea","for","2"]
-}

{-
    Exercise D
    head (map f xs)
    - eager evaluator, n
    - lazy evaluator, 1

    head . map f = f . head 

    first p = head . filter p
-}
first :: (a -> Bool) -> [a] -> a
first p xs | null xs = error "Empty list"
           | p x = x
           | otherwise = first p $ tail xs
           where x = head xs

-- Exercise E
first' :: (a -> Bool) -> [a] -> Maybe a
first' p xs | null xs = Nothing
            | p x = Just x
            | otherwise = first' p $ tail xs
            where x = head xs

-- Exercise F
exp' :: Integer -> Integer -> Integer
exp' x n | n == 0 = 1
         | n == 1 = x
         | otherwise = x * exp' x (n-1)

-- divide conquer technique
exp'' :: Integer -> Integer -> Integer
exp'' x n | n == 0 = 1
          | n == 1 = x
          | even n = evenMul
          | otherwise = evenMul * x-- odd
          where evenMul = exp'' x (n `div` 2) * exp'' x (n `div` 2)

-- Exercise G

type Date = (Int, Int, Int)

showDate :: Date -> String
showDate (d, m, y) = showDay d ++ " "++ month !! (m-1) ++ ", " ++ show y

showDay :: Int -> String
showDay d
    | d > 32 = error "Max number of day is 31"
    | otherwise = show d ++ suffix (d `mod` 10)

suffix :: Int -> String
suffix 1 = "st"
suffix 2 = "ed"
suffix 3 = "rd"
suffix _ = "th"

month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
