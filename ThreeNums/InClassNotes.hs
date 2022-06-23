module ThreeNums.InClassNotes where

{-
    3.3 Computing floors
    The value ⌊x⌋, the floor of x, is defined to be the largest integer m such that m ≤ x.
-}

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x = if p x then x else until' p f (f x)

-- fromInteger make sure x (-1) in until' is Integer
floor :: (Ord a, Num a) => a -> Integer
floor x = until' ((<=x) . fromInteger) (subtract 1) (-1)
