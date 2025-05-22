module Lib
    ( decodeMessage
    , decodeMessageImproved
    , parseInput
    , isMinUnique
    , isMaxUnique
    , isSumEven
    , midPoint
    , countMidPoint
    ) where

-- | Parse a string of space-separated integers into a list of integers.
--
-- >>> parseInput "1 2 3 4 5"
-- [1,2,3,4,5]
parseInput :: String -> [Int]
parseInput = map read . words

-- | Check if the minimum value in a list is unique.
--
-- >>> isMinUnique [1, 2, 3, 4, 5]
-- True
--
-- >>> isMinUnique [1, 2, 3, 1, 5]
-- False
isMinUnique :: [Int] -> Bool
isMinUnique xs = length (filter (== minimum xs) xs) == 1

-- | Check if the maximum value in a list is unique.
--
-- >>> isMaxUnique [1, 2, 3, 4, 5]
-- True
--
-- >>> isMaxUnique [1, 2, 3, 5, 5]
-- False
isMaxUnique :: [Int] -> Bool
isMaxUnique xs = length (filter (== maximum xs) xs) == 1

-- | Check if the sum of minimum and maximum values is even.
--
-- >>> isSumEven [1, 2, 3, 4, 5]
-- True
--
-- >>> isSumEven [1, 2, 3, 4, 6]
-- False
isSumEven :: [Int] -> Bool
isSumEven xs = (minimum xs + maximum xs) `mod` 2 == 0

-- | Calculate the midpoint between minimum and maximum values.
--
-- >>> midPoint [1, 2, 3, 4, 9]
-- 5
midPoint :: [Int] -> Int
midPoint xs = (minimum xs + maximum xs) `div` 2

-- | Count occurrences of the midpoint value in the list.
--
-- >>> countMidPoint [5, 5, 5, 8, 1, 2, 3, 4, 9, 8, 2, 3, 4]
-- 3
countMidPoint :: [Int] -> Int
countMidPoint xs = length $ filter (== midPoint xs) xs

-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Just 3
-- 
-- >>> decodeMessage "5 5 5 1 2 3 4 8 2 3"
-- Nothing
decodeMessage :: String -> Maybe Int
decodeMessage msg = 
  let nums = parseInput msg
  in if null nums
     then Nothing
     else if not (isMinUnique nums)
          then Nothing
          else if not (isMaxUnique nums)
               then Nothing
               else if not (isSumEven nums)
                    then Nothing
                    else Just (countMidPoint nums)

-- | Validate that minimum is unique
validateMinUnique :: [Int] -> Either String [Int]
validateMinUnique xs = 
  if isMinUnique xs
  then Right xs
  else Left "Communication interference detected: minimum number not Unique"

-- | Validate that maximum is unique
validateMaxUnique :: [Int] -> Either String [Int]
validateMaxUnique xs = 
  if isMaxUnique xs
  then Right xs
  else Left "Communication interference detected: maximum number not Unique"

-- | Validate that midpoint is even
validateSumEven :: [Int] -> Either String [Int]
validateSumEven xs = 
  if isSumEven xs
  then Right xs
  else Left "Communication interference detected: midPoint not even"

-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
-- This is an improved version of the previous function, with a more
-- informative error messages.
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Left "Communication interference detected: minimum number not Unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Left "Communication interference detected: maximum number not Unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Right 3
--
-- >>> decodeMessageImproved "5 5 5 1 2 3 4 8 2 3"
-- Left "Communication interference detected: midPoint not even"
decodeMessageImproved :: String -> Either String Int
decodeMessageImproved msg =
  Right (parseInput msg) >>= validateMinUnique >>= validateMaxUnique >>= validateSumEven >>= \xs -> Right (countMidPoint xs)