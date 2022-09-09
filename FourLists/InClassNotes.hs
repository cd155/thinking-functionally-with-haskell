module FourLists.InClassNotes where

{-
    4.1 List notation
-}

data List a = Nil | Cons a (List a)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- findPerfectNums x = head (filter perfect x)
--     where perfect n = (n == sum divisors n)

triads :: Int -> [(Int,Int,Int)]
triads n = [
    (x,y,z) | 
    x <- [1..n], y <- [1..n], z <- [1..n], 
    x*x+y*y==z*z]

divisors x = [d | d <- [2..x-1], x `mod` d == 0]
