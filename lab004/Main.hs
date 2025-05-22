import Data.List (maximumBy)
import Data.Function (on)
import Text.Printf (printf)

-- Task 1: Reverse a list
-- `mreverse` reverses a list recursively. If the list is empty, it returns an empty list.
-- Otherwise, it recursively reverses the tail of the list and appends the head to the reversed tail.
mreverse :: [a] -> [a]
mreverse [] = []  -- Base case: empty list
mreverse (x:xs) = mreverse xs ++ [x]  -- Recursively reverse the tail and append the head

-- Task 2: Print a multiplication table
-- `mulTable` generates and prints a multiplication table of size `n x n`.
-- It creates a list of strings where each string represents a row of the table.
-- The `pad` function is used to format the numbers to align them properly.
mulTable :: Int -> IO ()
mulTable n = mapM_ putStrLn [unwords [pad (x * y) | y <- [1..n]] | x <- [1..n]]
-- Generate a list of rows. Each row contains the product of `x` and `y` for all `y` in the range [1..n]
-- `pad` ensures the numbers are aligned, and `unwords` combines the numbers into a single string per row.

-- Utility function to format numbers for proper alignment in the table
pad :: Int -> String
pad x = printf "%3d" x  -- Format each number to take up exactly 3 spaces for neat alignment

-- Task 3: Count how many students have the oldest age
-- The input `String` contains multiple lines, each with a student's name and age.
-- The function splits the input into lines, extracts the ages, and then counts how many students share the oldest age.
oldestStudentsCount :: String -> Int
oldestStudentsCount input =
    let students = map words (lines input)  -- Split the input into lines (one per student) and words
        -- `map (read . last)` extracts the last word (the age) from each student's data and converts it to an integer
        (maxAge, count) = foldl countMaxAge (minBound, 0) students
            where
                -- `countMaxAge` is a function that keeps track of the maximum age and the count of students with that age
                countMaxAge (maxSoFar, countSoFar) student =
                    let age = read (last student) :: Int  -- Extract the student's age from the last word in their data
                    in if age > maxSoFar
                        then (age, 1)  -- If a new max age is found, update and reset the count to 1
                        else if age == maxSoFar
                            then (maxSoFar, countSoFar + 1)  -- If the age matches the current max, increment the count
                            else (maxSoFar, countSoFar)  -- Otherwise, leave the count unchanged
    in count  -- Return the count of students with the oldest age

-- Main function to run tests
main :: IO ()
main = do
    putStrLn "Haskell Lab 04"  -- Print the name of the lab
    putStrLn "Reversing 'Hello':"  -- Introduce the reverse test
    print (mreverse "Hello")  -- Print the result of reversing the string "Hello"
    
    putStrLn "\nMultiplication Table (5x5):"  -- Introduce the multiplication table test
    mulTable 8  -- Print a 5x5 multiplication table

    putStrLn "\nTesting Oldest Students Count with file 'students.txt':"  -- Introduce the oldest students count test
    content <- readFile "students.txt"  -- Read the content of the file "students.txt"
    print (oldestStudentsCount content)  -- Print the number of students with the oldest age

-- Complexity:
-- Task 1: mreverse has O(n) complexity as it processes each element once.
-- Task 2: mulTable has O(n^2) complexity since it generates an n x n table and processes every element.
-- Task 3: The optimized `oldestStudentsCount` has O(n) complexity as it makes a single pass through the data to find the maximum age and count occurrences.
