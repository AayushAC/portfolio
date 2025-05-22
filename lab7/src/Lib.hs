module Lib
    ( countScore
    , processLine
    , basePoints
    , winningMultiplier
    , countOccurrences
    , calculatePointsForMatch
    , calculateScoreForGame
    , unique
    ) where

-- | Count the total score of the five balls lottery game data.
-- The input data is a text string with many games. Each game is represented as a single line.
-- The first three numbers are the "Winning Numbers", and the next five are the lottery numbers.
-- Each row ends with an end-of-line character.
countScore :: String -> Int
countScore txt = sum $ map processLine $ lines txt

-- | Process a single line of the input data.
-- Each line represents a game, the first three numbers are the "Winning Numbers", 
-- and the next five are the lottery numbers. Calculate the score for the single game.
--
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5
--
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 7
--
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 11
--
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8
--
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0
--
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56
--
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9
-- 
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3
--
-- >>> processLine "35 35 35 1 5 6 35 16"
-- 32
--
-- >>> processLine "35 35 6 1 5 6 35 35"
-- 49
-- 
-- >>> processLine "35 35 35 1 5 6 35 35"
-- 96
processLine :: String -> Int
processLine line = calculateScoreForGame winningNumbers lotteryNumbers
  where
    allNumbers = map read $ words line :: [Int]
    winningNumbers = take 3 allNumbers
    lotteryNumbers = drop 3 allNumbers

-- | Calculate the base points for a number based on its value.
-- Uses the exponential sequence: 1, 2, 4, 8, ... i.e. 2^(n `div` 10).
--
-- >>> basePoints 5
-- 1
--
-- >>> basePoints 15
-- 2
--
-- >>> basePoints 25
-- 4
--
-- >>> basePoints 35
-- 8
basePoints :: Int -> Int
basePoints n = 2 ^ (n `div` 10)

-- | Calculate the multiplier for a winning number based on its occurrences
-- in the winning numbers list. It returns 2^(occurrence - 1) if the number occurs
-- at least once, and 0 otherwise.
--
-- >>> winningMultiplier 10 [10, 20, 30]
-- 1
--
-- >>> winningMultiplier 10 [10, 10, 30]
-- 2
--
-- >>> winningMultiplier 10 [10, 10, 10]
-- 4
winningMultiplier :: Int -> [Int] -> Int
winningMultiplier n winningNums =
  let occ = countOccurrences n winningNums
  in if occ >= 1 && occ <= 3 then 2 ^ (occ - 1) else 0

-- | Count occurrences of a number in a list.
--
-- >>> countOccurrences 5 [1, 5, 3, 5, 7]
-- 2
countOccurrences :: Int -> [Int] -> Int
countOccurrences n = length . filter (== n)

-- | Calculate score for matches of a single winning number.
-- It computes the initial score as (basePoints * winningMultiplier) and then
-- doubles the points for each occurrence of the winning number in the lottery numbers.
calculatePointsForMatch :: Int -> [Int] -> [Int] -> Int
calculatePointsForMatch num winningNums lotteryNums =
  let base = basePoints num
      multiplier = winningMultiplier num winningNums
      matches = filter (== num) lotteryNums
      
      -- Calculate points with doubling for each occurrence.
      calculatePoints [] _ = 0
      calculatePoints (x:xs) currentPoints = 
        currentPoints + calculatePoints xs (currentPoints * 2)
  in
      calculatePoints matches (base * multiplier)

-- | Calculate the total score for a game.
calculateScoreForGame :: [Int] -> [Int] -> Int
calculateScoreForGame winningNums lotteryNums =
  let uniqueWinningNums = filter (`elem` lotteryNums) $ unique winningNums
  in sum $ map (\n -> calculatePointsForMatch n winningNums lotteryNums) uniqueWinningNums

-- | Get unique elements of a list.
--
-- >>> unique [1, 2, 3, 2, 1]
-- [1,2,3]
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

-- | Example usage:
--   cat data.txt | stack exec lab07-exe
