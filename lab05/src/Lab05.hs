module Lab05 where

import Control.Exception (evaluate)

-- Task 1: mhead - first element of a list
-- Implementation 1: Using function argument pattern matching
-- | Returns the first element of a list
-- >>> mhead1 [1,2,3]
-- 1
-- >>> mhead1 "hello"
-- 'h'
-- >>> mhead1 []
-- *** Exception: mhead1: empty list
mhead1 :: [a] -> a
mhead1 (x:_) = x
mhead1 [] = error "mhead1: empty list"

-- Interactive wrapper for GHCi
mheadInteractive :: IO ()
mheadInteractive = do
  putStrLn "Enter a list of numbers (e.g., [1,2,3]): "
  input <- getLine
  let result = mhead1 (read input :: [Int])
  print result

-- Implementation 2: Using function guards
mhead2 :: [a] -> a
mhead2 xs 
  | null xs = error "mhead2: empty list"
  | otherwise = xs !! 0

-- Implementation 3: Single line with if-else
mhead3 :: [a] -> a
mhead3 xs = if null xs then error "mhead3: empty list" else xs !! 0

-- Implementation 4: Using let-in
mhead4 :: [a] -> a
mhead4 xs = 
  let isEmpty = null xs
      firstElement = xs !! 0
  in if isEmpty then error "mhead4: empty list" else firstElement

-- Implementation 5: Using where
mhead5 :: [a] -> a
mhead5 xs = if isEmpty then error "mhead5: empty list" else firstElement
  where
    isEmpty = null xs
    firstElement = xs !! 0

-- Implementation 6: Using case-of
mhead6 :: [a] -> a
mhead6 xs = case xs of
  [] -> error "mhead6: empty list"
  (x:_) -> x

-- Task 2: factorial function
-- | Returns the factorial of a number
-- >>> mfact 5
-- 120
-- >>> mfact 0
-- 1
-- >>> mfact 1
-- 1
-- >>> mfact 3
-- 6
mfact :: Integer -> Integer
mfact 0 = 1
mfact n = n * mfact (n - 1)

-- Interactive factorial wrapper
mfactInteractive :: IO ()
mfactInteractive = do
  putStrLn "Enter a number to compute factorial: "
  input <- getLine
  let result = mfact (read input)
  print result

-- Task 3: fibonacci function
-- | Returns the nth Fibonacci number
-- >>> fib 0
-- 0
-- >>> fib 1
-- 1
-- >>> fib 2
-- 1
-- >>> fib 3
-- 2
-- >>> fib 6
-- 8
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Interactive Fibonacci wrapper
fibInteractive :: IO ()
fibInteractive = do
  putStrLn "Enter an index for Fibonacci sequence: "
  input <- getLine
  let result = fib (read input)
  print result

-- Task 4: fibonacci sequence
-- | Infinite list of Fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

-- | Returns the nth Fibonacci number using the infinite list
-- >>> fib2 0
-- 0
-- >>> fib2 1
-- 1
-- >>> fib2 2
-- 1
-- >>> fib2 3
-- 2
-- >>> fib2 6
-- 8
fib2 :: Int -> Integer
fib2 n = fibs !! n


-- Interactive Fibonacci (fast version)
fib2Interactive :: IO ()
fib2Interactive = do
  putStrLn "Enter an index for Fibonacci sequence (fast version): "
  input <- getLine
  let result = fib2 (read input)
  print result

-- Bonus task helper (for reference)
-- | Counts occurrences of a value in a list
-- >>> count 10 [2,10,3,10,4]
-- 2
-- >>> count 'a' "banana"
-- 3
count :: Eq a => a -> [a] -> Int
count x xs = length (filter (== x) xs)

-- Interactive count function
countInteractive :: IO ()
countInteractive = do
  putStrLn "Enter a value to count in the list: "
  value <- getLine
  putStrLn "Enter a list (e.g., [1,2,3,1]): "
  lst <- getLine
  let result = count (read value) (read lst :: [Int])
  print result
