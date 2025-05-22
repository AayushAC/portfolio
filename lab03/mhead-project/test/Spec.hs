{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Main
Description : Test suite for the MHead module using HSpec and QuickCheck.
              This test suite verifies that each custom mhead implementation behaves the same
              as the built-in 'head' function when applied to non-empty lists.
-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import MHead
import Prelude  -- Ensure the built-in head is in scope

main :: IO ()
main = hspec $ do
  describe "mhead implementations" $ do
    -- For each implementation, we check that for any non-empty list,
    -- the result of mheadX equals the result of the built-in head.
    it "mhead1 behaves like head for non-empty lists" $
      property $ \(xs :: [Int]) ->
        not (null xs) ==> (mhead1 xs == head xs)
    it "mhead2 behaves like head for non-empty lists" $
      property $ \(xs :: [Int]) ->
        not (null xs) ==> (mhead2 xs == head xs)
    it "mhead3 behaves like head for non-empty lists" $
      property $ \(xs :: [Int]) ->
        not (null xs) ==> (mhead3 xs == head xs)
    it "mhead4 behaves like head for non-empty lists" $
      property $ \(xs :: [Int]) ->
        not (null xs) ==> (mhead4 xs == head xs)
    it "mhead6 behaves like head for non-empty lists" $
      property $ \(xs :: [Int]) ->
        not (null xs) ==> (mhead6 xs == head xs)
    it "mhead7 behaves like head for non-empty lists" $
      property $ \(xs :: [Int]) ->
        not (null xs) ==> (mhead7 xs == head xs)

{- 
Additional Explanation:
-------------------------
This test suite is built using HSpec and QuickCheck:
  - HSpec organizes the tests into a human-readable structure with descriptions.
  - QuickCheck generates a variety of test cases automatically, verifying properties of the functions.
  
Each test checks that for any non-empty list (ensured by the guard 'not (null xs)'), the custom mhead
functions (mhead1, mhead2, etc.) return the same result as the built-in 'head' function.

Key Points:
- Property-based testing with QuickCheck helps ensure robustness by testing many random cases.
- The tests are restricted to non-empty lists to avoid runtime errors from partial functions.
- This approach gives confidence that the implementations are correct for the intended input domain.
-}


--In order to run the test suite, you can use the following command:
--stack build
--stack test