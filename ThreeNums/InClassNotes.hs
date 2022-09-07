module ThreeNums.InClassNotes where

{-
    3.3 Computing floors
    The value ⌊x⌋, the floor of x, is defined to be the largest integer m such that m ≤ x.
-}

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x = if p x then x else until' p f (f x)

{-
    fromInteger make sure x (-1) in until' is Integer
    x is (-1) because we assume x < 0. Therefore, this is the first case we want to check
-}

floor' :: (Ord a, Num a) => a -> Integer
floor' x 
    | x < 0 =  floorSmZero x
    | otherwise = floorGtrZero x

floorSmZero :: (Ord a, Num a) => a -> Integer
floorSmZero x = until' ((<=x) . fromInteger) (subtract 1) (-1)

floorGtrZero :: (Ord a, Num a) => a -> Integer
floorGtrZero x = until' ((>=x) . fromInteger) (+ 1) 0

-- leq: input less equal to x
-- input is -1
leq :: Integer -> Float -> Bool
leq m x = fromInteger m <= x

-- lt: x less than input
-- input is 1
lt :: Float -> Integer -> Bool
lt x n = x < fromInteger n

floor x = if x < 0
        then until (`leq` x) (subtract 1) (-1)
        else until (x `lt`) (+1) 1 - 1
        where m `leq` x = fromInteger m <= x
              x `lt` n = x < fromInteger n

floor'''' :: Float -> Integer
floor'''' x = fst (until unit (shrink x) (bound x))
    where unit (m,n) = m+1 == n

type Interval = (Integer,Integer)
shrink :: Float -> Interval -> Interval
shrink x (m,n) = if p `leq` x then (p,n) else (m,p)
    where p = choose (m,n)

choose :: Interval -> Integer
choose (m,n) = (m+n) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

lower :: Float -> Integer
lower x = until (`leq` x) (*2) (-1)

upper :: Float -> Integer
upper x = until (x `lt`) (*2) 1

floor''''' x = if r < 0 then n-1 else n
    where (n,r) = properFraction x
