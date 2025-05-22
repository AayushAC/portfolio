-- Lab03: Haskell Basics
main :: IO ()
main = do
    task1
    putStrLn " "
    name <- task2    -- Capture the name from task2
    putStrLn " "
    task3 name       -- Pass the name to task3

-- Task 1: Hello world
task1 :: IO ()
task1 = putStrLn "Hello, world!"

-- Task 2: Hello Name
task2 :: IO String
task2 = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)
    return name       -- Return the name so it can be used in task3

-- Task 3: Age (takes name as a parameter)
task3 :: String -> IO ()
task3 name = do
    putStrLn ("Hi, " ++ name ++ ", What is your age?")
    ageStr <- getLine
    let age = read ageStr :: Int
    let futureAge1 = addNumber age 10  -- Using addNumber for the future age calculation
    let futureAge2 = addAge (Age age) (Age 10)  -- Using addAge for the future age calculation (Version A)
    let futureAge3 = addAgeV2 age 10  -- Using addAgeV2 for the future age calculation (Version B)
    putStrLn ("Hello " ++ name ++ ", in 10 years you will be " ++ show futureAge1 ++ " (using addNumber).")
    putStrLn ("Hello " ++ name ++ ", in 10 years you will be " ++ show (getAge futureAge2) ++ " (using addAge).")
    putStrLn ("Hello " ++ name ++ ", in 10 years you will be " ++ show (getAge futureAge3) ++ " (using addAgeV2).")

-- Type Safety Functions

-- Function that adds two numbers of any numeric type
addNumber :: Num a => a -> a -> a
addNumber x y = x + y

-- Creating a new type 'Age' using 'newtype'
newtype Age = Age Int

-- Function that adds two 'Age' values together (Version A: strict type safety)
addAge :: Age -> Age -> Age
addAge (Age a) (Age b) = Age (a + b)

-- Version B: Allowing addAge to work with `Int` and wrapping it in `Age`
addAgeV2 :: Int -> Int -> Age
addAgeV2 a b = Age (a + b)

-- A helper function to extract the `Int` value from `Age`
getAge :: Age -> Int
getAge (Age a) = a


-- Version A: Strict type safety, only works with Age values.
-- Version B: More flexible, accepts Int and converts to Age.
