module Exercises.ExerciseThree where
import Distribution.Simple.Utils (xargs)

{-
    Exercise A
    -2 + 3
    3 + -2 error: + and - have the same infixl level
    3 + (-2)
    subtract 2 3
    2 + subtract 3 error: subtract 3 is a function not Num
    flip :: (a -> b -> c) -> b -> a -> c, subtract 2 3 -> subtract 3 2
-}

{-
    Exercise E
-}

-- leq: input less equal to x
-- input is -1
leq :: Integer -> Float -> Bool
leq m x = fromInteger m <= x

-- lt: x less than input
-- input is 1
lt :: Float -> Integer -> Bool
lt x n = x < fromInteger n

isqrt :: Float -> Integer
isqrt x = fst (until unit (shrink x) (bound x))
    where unit (m,n) = (m+1 == n)

type Interval = (Integer,Integer)

shrink :: Float -> Interval -> Interval
shrink x (m,n) = if (p*p) `leq` x then (p,n) else (m,p)
    where p = (m+n) `div` 2

bound :: Float -> Interval
bound x = (0,until above (*2) 1)
    where above n = x `lt` (n*n)

{-
    Exercise F
    x^2 = a
    x1 start with some guess x > 0
    Newton method: x n+1 = 1/2 * (xn + (a / xn))
    https://math.mit.edu/~stevenj/18.335/newton-sqrt.pdf
-}

sqrt' :: Float -> Float
sqrt' x = until tolerance newtonMethod x
    where newtonMethod y = 0.5 * (y + x/y)
          tolerance y = abs (y*y - x) < epison * x
          epison = 0.01
          -- for test purpose let guess start with x/3

data Nat = Zero | Succ Nat

instance Ord Nat where
    Zero < Zero = False -- backward is the same
    Zero < Succ n = True -- backward is not the same
    Succ n < Zero = False
    Succ n < Succ m = n < m -- backward is the same

instance Eq Nat where
    Zero == Zero = True 
    Zero == Succ n = False 
    Succ m == Zero = False 
    Succ m == Succ n = m == n 

-- require Eq instance
instance Num Nat where
    m + Zero = m
    m + Succ n = Succ (m+n)
    m * Zero = Zero
    m * (Succ n) = m * n + m
    
    abs n = n
    signum Zero = Zero
    signum (Succ n) = Succ Zero
    
    m - Zero = m
    Zero - Succ n = Zero
    Succ m - Succ n = m - n
    
    fromInteger x
        | x <= 0 = Zero
        | otherwise = Succ (fromInteger (x-1))

divMod' :: Nat -> Nat -> (Nat,Nat)
divMod' x y = if x < y then (Zero,x)
              else (Succ q,r)
              where (q,r) = divMod' (x-y) y
