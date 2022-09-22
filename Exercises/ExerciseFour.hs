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

{-
    Exercise F
    data List a = Nil | Cons a (List a)
    Snoc is Cons backwards
    [1,2,3] = Cons 1 (Cons 2 (Cons 3 Nil))
    [1,2,3] = Snoc (Snoc (Snoc Nil 1) 2) 3
-}

data List a = Nil | Snoc (List a) a deriving Show

head' :: List a -> a 
head' Nil = error "Empty List"
head' (Snoc Nil x) = x
head' (Snoc xs x)  = head' xs

last' :: List a -> a
last' Nil        = error "Empty List"
last' (Snoc _ x) = x

toList :: [a] -> List a
toList = toList' . reverse -- reverse is necessary

toList' :: [a] -> List a
toList' []     = Nil
toList' [x]    = Snoc Nil x
toList' (x:xs) = Snoc (toList' xs) x

fromList :: List a -> [a]
fromList = reverse .fromList'

fromList' :: List a -> [a]
fromList' Nil          = []
fromList' (Snoc Nil x) = [x]
fromList' (Snoc xs x)  = x : fromList' xs

length' :: [a] -> Int
length' xs = loop (0,xs)
    where loop (n,[])   = n
          loop (n,x:xs) = loop (n+1,xs)

{-
    Exercise H
    take 0 undefined => []
    take undefined [] => error

    take n xs ++ drop n xs = xs, good
    take m . drop n = drop n . take(m+n), bad when m be positive and n be negative 
    take m . take n = take (m `min` n)
    drop m. drop n = drop (m+n)
-} 

-- splitAt n xs = (take n xs, drop n xs)
mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt n xs
    | n < 0 = splitAt' 0 xs [] 0
    | otherwise = splitAt' n xs [] 0

splitAt' :: Integer -> [a] -> [a] -> Integer -> ([a], [a])
splitAt' n [] buildup count = (buildup, [])
splitAt' n (x:xs) buildup count
    | n == count = (buildup, x:xs)
    | otherwise = splitAt' n xs (buildup ++ [x]) (count+1) 
