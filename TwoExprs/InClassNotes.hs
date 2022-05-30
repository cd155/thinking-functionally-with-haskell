module TwoExprs.InClassNotes where

{-
    2.2 Names and operators
    A script is a collection of names and their definitions

    Section: 
    (+ 1) n = n + 1
    (0 <) n = 0< n
    (< 0) n = n < 0
    (1 /) x = 1/x

    (-1) is just negative 1
-}

-- Declare two functions at one time
succ, double :: Integer -> Integer
succ n   = n + 1
double n = 2 * n

{-
    2.3 Evaluation
    Haskell evaluates an expression by reducing it to its simplest possible 
    form and printing the result.

    With eager evaluation (innermost reduction) arguments are always evaluated 
    before a function is applied. With lazy evaluation (outermost reduction) 
    the definition of a function is installed at once and only when they are 
    needed are the arguments to the function evaluated.

    The pros and cons of lazy evaluation are briefly as follows. On the plus 
    side, lazy evaluation terminates whenever any reduction order terminates; 
    it never takes more steps than eager evaluation, and sometimes infinitely 
    fewer. On the minus side, it can require a lot more space and it is more 
    difficult to understand the precise order in which things happen.
    - Haskell uses lazy evaluation. 
    - ML (another popular functional language) uses eager evaluation.

    A Haskell function f is said to be strict if f undefined = undefined. 
    Otherwise, the function is said to be non-strict. Lazy evaluation lets 
    Haskell define non-strict functions, such as the three function:

    For example:
        > three x = 3
        > three undefined
        3
    so Haskell allows non-strict functions.
-}
three :: Integer -> Integer   -- a non-strict function
three x = 3

{-
    2.4 Types and type classes
    The type Bool has three values, not two: False, True and undefined :: Bool.
-}

to :: Bool -> Bool -- never going to end
to b = not (to b)

{-
    2.5 Printing values
-}

commonWords :: Int -> String -> String
commonWords _ _ = ""

main :: IO ()
main = do {putStrLn "Take text from where:";
    infile <- getLine;
    putStrLn "How many words:";
    n <- getLine;
    putStrLn "Put results where:";
    outfile <- getLine;
    text <- readFile infile;
    writeFile outfile (commonWords (read n) text);
    putStrLn "cwords done!" }
