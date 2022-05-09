module OneBasics.Exercises where
import Data.Char (toUpper)

{-
    Exercise A
    map double [1,4,4,3]            = [2,8,8,6]
    map (double . double) [1,4,4,3] = [4,16,16,12]
    map double []                   = []

    sum . map double = double . sum
    sum . map sum    = sum . concat
    sum . sort       = sum
-}

double :: Integer -> Integer
double x = x * 2

{-
    Exercise B
    In Haskell, functional application takes precedence over every other operator, 
    so double 3+4 means (double 3)+4, not double (3+4).

    sin²θ      = sin theta ^ 2 = (sin theta) ^ 2
    (sin 2θ) / 2π = sin (2 * θ) / (2 * π)
-}

{-
    Exercise D

    words :: [Char] -> [[Char]]
    toLower :: Char -> Char
    map :: (a -> b) -> [a] -> [b]

    words . map toLower = map (map toLower) words
-}

{-
    Exercise E
    An operator ⊕ is said to be associative if x ⊕ (y ⊕ z) = (x ⊕ y) ⊕ z
    Is numerical addition associative?      Yes
    Is list concatenation associative?      Yes
    Is functional composition associative?  No
    Give an example of an operator on numbers that is not associative. mod (reminder)

    An element e is said to be an identity element of ⊕ if x⊕e = e⊕x = x for all x.
    Identity elements for
        Addition:               0
        concatenation:          []
        functional composition: id :: a -> a
                                id x = x
-}

{-
    Exercise F: Great exercise.
    Make sure understand the problem before start to solve it.
-}

{-
    Exercise G
-}

units = ["zero","one","two","three","four","five",
    "six","seven","eight","nine"]

song :: Int -> String
song n
    | n == 0    = ""
    | otherwise = trimNewline $ song (n-1) ++ "\n" ++ verse n

trimNewline :: String -> String
trimNewline [] = []
trimNewline (x:xs) 
    | x == '\n' = xs
    | otherwise = x:xs

verse :: Int -> String
verse n = line1 n ++ line2And4 ++ line3 n ++ line2And4

line1 :: Int -> String
line1 n = capitalize $ units !! n ++ " " ++ man ++ " went to mow\n"
    where man = if n == 1 then "man" else "men"

line2And4 ::  String
line2And4 = "Went to mow a meadow\n"

line3 :: Int -> String
line3 n = capitalize $ countMan n ++ " and his dog\n"

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs 

countMan :: Int -> String
countMan n
    | n == 1 = units !! n ++ " man"
    | otherwise = units !! n ++ " men, " ++ countMan (n-1)
