module OneBasics.Exercises where

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

    sin²θ      = (sin theta)^2
    sin 2θ /2π = sin ((2 * θ) / (2 * π))
-}
