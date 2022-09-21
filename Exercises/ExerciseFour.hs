module Exercises.ExerciseFour where

-- Exercise B unique pairs 
allPairs m n = [ (x,y) | x <- [0..m],
                         y <- [x..n] ]

-- Exercise C determine whether has common elements 
isDisjoint :: (Ord a) => [a] -> [a] -> Bool
isDisjoint a b = null ([ x | x <- a,
                             y <- b,
                             x == y ])

{-
    Exercise D
    [e | x <- xs, p x, y <- ys]
    [e | x <- xs, y <- ys, p x]

    test1:
    [1 | x <- [1,3], even x, y <- undefined]
    [1 | x <- [1,3], y <- undefined, even x]
    
    test2:
    [1 | x <- [1,3], even x, y <- [1..]]
    [1 | x <- [1,3], y <- [1..], even x]

    order is matter, haskell evaluate each variable in order
-}

-- Exercise E
twoCubeNumbersUpperBound x = until ((>= x). length . listTwoCubeNumbers) (+ 1) 0

listTwoCubeNumbers x = [ (a, b , c, d) | a <- [1..x], 
                                      b <- [a..x], 
                                      c <- [a..x], 
                                      d <- [c..x], 
                                      (a*a*a + b*b*b) == (c*c*c + d*d*d),
                                      a /= c,
                                      -- a /= d,
                                      -- b /= c, 
                                      b /= d ]

-- Exercise F
