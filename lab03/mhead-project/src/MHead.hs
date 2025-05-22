{-# LANGUAGE LambdaCase #-}
{-|
Module      : MHead
Description : Different implementations of the head function.
              This module provides several versions of a function that returns
              the first element of a list (similar to the built-in 'head').
              Each implementation uses a different approach to illustrate various
              Haskell techniques such as pattern matching, lambda-case, list indexing,
              folding, and working with the Maybe type.
-}

module MHead
  ( mhead1
  , mhead2
  , mhead3
  , mhead4
  , mhead6
  , mhead7
  ) where

import Data.Maybe (fromJust, listToMaybe)

-- | mhead1 returns the first element of a list using simple pattern matching.
--
--   Example:
--   >>> mhead1 [1,2,3]
--   1
--
--   Explanation:
--   This function checks if the list is empty. If it is, an error is thrown.
--   Otherwise, it matches (x:_) and returns the first element 'x'.
mhead1 :: [a] -> a
mhead1 []    = error "mhead1: empty list"  -- Handle empty list by throwing an error.
mhead1 (x:_) = x                           -- Return the first element.

-- | mhead2 uses lambda-case for pattern matching.
--
--   Example:
--   >>> mhead2 [1,2,3]
--   1
--
--   Explanation:
--   Lambda-case allows pattern matching in an anonymous function.
--   This function handles the empty list case by throwing an error, and returns
--   the first element for a non-empty list.
mhead2 :: [a] -> a
mhead2 = \case
  []    -> error "mhead2: empty list"  -- If the list is empty, throw an error.
  (x:_) -> x                         -- For non-empty list, return the first element.

-- | mhead3 obtains the first element by indexing into the list.
--
--   Example:
--   >>> mhead3 [1,2,3]
--   1
--
--   Explanation:
--   The (!!) operator is used to get the element at index 0.
--   This is a direct way to access the first element, though it is partial
--   (it will fail if the list is empty).
mhead3 :: [a] -> a
mhead3 xs = xs !! 0  -- Returns the element at index 0.

-- | mhead4 uses foldr to extract the first element.
--
--   Example:
--   >>> mhead4 [1,2,3]
--   1
--
--   Explanation:
--   foldr traverses the list from the right, but the provided lambda
--   ignores the second argument and always returns the current element.
--   The initial accumulator is an error for an empty list.
mhead4 :: [a] -> a
mhead4 xs = foldr (\x _ -> x) (error "mhead4: empty list") xs

-- | mhead6 uses 'listToMaybe' together with 'fromJust'.
--
--   Example:
--   >>> mhead6 [1,2,3]
--   1
--
--   Explanation:
--   'listToMaybe' converts a list to a Maybe type (Nothing for empty list, Just x for non-empty).
--   'fromJust' then extracts the value from the Just. Note that if the list is empty,
--   fromJust will throw an error.
mhead6 :: [a] -> a
mhead6 xs = fromJust (listToMaybe xs)

-- | mhead7 extracts the first element using the 'take' function and pattern matching.
--
--   Example:
--   >>> mhead7 [1,2,3]
--   1
--
--   Explanation:
--   'take 1 xs' returns a list with at most one element.
--   The case expression then matches this list: if it's empty, an error is thrown;
--   if it has an element, that element is returned.
mhead7 :: [a] -> a
mhead7 xs = case take 1 xs of
  []    -> error "mhead7: empty list"  -- If take returns an empty list, throw an error.
  (y:_) -> y                          -- Otherwise, return the first element.

{- 
Additional Explanation:
-------------------------
This module demonstrates various techniques for implementing a function that returns the first element
of a list. Each implementation uses a different approach:
  - Direct pattern matching (mhead1)
  - Lambda-case for inline pattern matching (mhead2)
  - List indexing (mhead3)
  - Folding with foldr (mhead4)
  - Conversion to Maybe and extraction (mhead6)
  - Using take and case expression (mhead7)

All these functions are partial in the sense that they throw an error if the list is empty.
This serves as a practical demonstration of Haskell's pattern matching and higher-order function capabilities.
-}
