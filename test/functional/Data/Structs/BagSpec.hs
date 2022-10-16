{- |
   Module      : Data.Structs.BagSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Data.Structs.BagSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Structs.Bag

spec :: Spec
spec = do
  describe "A bag is a set that allows duplicates and where order does not matter" $ do
    it "a bag is a functor" $
      fmap (\ x -> x * 2) (fromList [1, 2, 3]) `shouldBe` (fromList ([2, 4, 6] :: [Int]))

    it "a bag is foldable" $
      sum (fromList [1, 2, 3]) `shouldBe` (6 :: Int)

    it "empty bags are equals" $
      (fromList ([] :: [Int])) == (fromList ([] :: [Int])) `shouldBe` True

    it "bags with different elements are differents" $
      (fromList "foo") == (fromList "") `shouldBe` False

    it "bags with the same elements in different orders are equals (1)" $
      (fromList "bar") == (fromList "rba") `shouldBe` True

    it "bags with the same elements in different orders are equals (2)" $
      (fromList "babar") == (fromList "arbba") `shouldBe` True

    it "bags with the same unique elements are not necessarily equals" $
      (fromList "foo") == (fromList "foofoo") `shouldBe` False
