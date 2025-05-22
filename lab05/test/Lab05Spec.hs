module Lab05Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Exception (evaluate)
import Lab05

spec :: Spec
spec = do
  describe "mhead functions" $ do
    it "returns the first element of a non-empty list" $ do
      mhead1 [1,2,3] `shouldBe` 1
      mhead2 [1,2,3] `shouldBe` 1
      mhead3 [1,2,3] `shouldBe` 1
      mhead4 [1,2,3] `shouldBe` 1
      mhead5 [1,2,3] `shouldBe` 1
      mhead6 [1,2,3] `shouldBe` 1
    
    it "throws an error on empty list" $ do
      evaluate (mhead1 []) `shouldThrow` anyErrorCall
      evaluate (mhead2 []) `shouldThrow` anyErrorCall
      evaluate (mhead3 []) `shouldThrow` anyErrorCall
      evaluate (mhead4 []) `shouldThrow` anyErrorCall
      evaluate (mhead5 []) `shouldThrow` anyErrorCall
      evaluate (mhead6 []) `shouldThrow` anyErrorCall
  
  describe "mfact function" $ do
    it "calculates factorial correctly" $ do
      mfact 0 `shouldBe` 1
      mfact 1 `shouldBe` 1
      mfact 5 `shouldBe` 120
      
  describe "fibonacci functions" $ do
    it "calculates fibonacci numbers correctly" $ do
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 6 `shouldBe` 8
      
    it "calculates fibonacci using sequence correctly" $ do
      fib2 0 `shouldBe` 0
      fib2 1 `shouldBe` 1
      fib2 2 `shouldBe` 1
      fib2 6 `shouldBe` 8
      
    it "both fibonacci functions return same results" $ property $
      \n -> n >= 0 && n < 20 ==> fib (fromIntegral n) == fib2 n
      
  describe "bonus functions" $ do
    it "count function works correctly" $ do
      count 10 [2,10,3,10,4] `shouldBe` 2
      count 'a' "banana" `shouldBe` 3