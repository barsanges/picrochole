{- |
   Module      : Picrochole.Data.LayoutSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Teste le module Picrochole.Data.Layout.
-}

module Picrochole.Data.LayoutSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe ( fromJust, isJust, isNothing )
import qualified Data.Set as S
import Picrochole.Data.Keys
import Picrochole.Data.Layout

testLocs :: [Location]
testLocs = [ l { height = i } | i <- [0..19] :: [Int] ]
  where
    l = Loc { ground = Plain
            , height = 0
            , lCivilianSupply = 0
            }

testCLoc :: CLocations
testCLoc = fromJust (fromList 5 testLocs)

spec :: Spec
spec = do
  describe "fromList" $ do
    it "should fail if the elements in the list cannot be distributed among the columns" $
      isNothing (fromList 3 testLocs) `shouldBe` True

    it "should succeed if the elements in the list can be distributed among the columns" $
      isJust (fromList 5 testLocs) `shouldBe` True

  describe "lLookup" $ do
    it "should fail if the index is invalid (1)" $
      fmap height (lLookup testCLoc (LK 5 0)) `shouldBe` Nothing

    it "should fail if the index is invalid (2)" $
      fmap height (lLookup testCLoc (LK 1 4)) `shouldBe` Nothing

    it "should fail if the index is invalid (3)" $
      fmap height (lLookup testCLoc (LK (-1) 2)) `shouldBe` Nothing

    it "returns the value for the given index (1)" $
      fmap height (lLookup testCLoc (LK 0 0)) `shouldBe` (Just 0)

    it "returns the value for the given index (2)" $
      fmap height (lLookup testCLoc (LK 0 2)) `shouldBe` (Just 10)

    it "returns the value for the given index (3)" $
      fmap height (lLookup testCLoc (LK 4 3)) `shouldBe` (Just 19)

  describe "dist" $ do
    it "computes the distance between two cells (1)" $
      dist (LK 0 0) (LK 0 0) `shouldBe` 0

    it "computes the distance between two cells (2)" $
      dist (LK 1 2) (LK 1 2) `shouldBe` 0

    it "computes the distance between two cells (3)" $
      dist (LK 1 1) (LK 1 2) `shouldBe` 1

    it "computes the distance between two cells (4)" $
      dist (LK 1 1) (LK 3 2) `shouldBe` 2

    it "computes the distance between two cells (5)" $
      dist (LK 1 1) (LK 0 4) `shouldBe` 3

    it "should always return a positive result" $ property $
      \ x1 x2 y1 y2 -> (dist (LK x1 x2) (LK y1 y2)) >= 0

  describe "neighbors" $ do
    it "gives the neighbors of a given cell (1)" $
      neighbors testCLoc (LK 0 0) 0 `shouldBe` [ LK 0 0 ]

    it "gives the neighbors of a given cell (2)" $
      neighbors testCLoc (LK 1 1) 1 `shouldBe` [ LK 0 1
                                               , LK 1 2
                                               , LK 1 0
                                               , LK 1 1
                                               , LK 2 2
                                               , LK 2 0
                                               , LK 2 1
                                               ]

    it "gives the neighbors of a given cell (3)" $
      neighbors testCLoc (LK 0 0) 1 `shouldBe` [ LK 0 0
                                               , LK 0 1
                                               , LK 1 0
                                               ]

    it "gives the neighbors of a given cell (4)" $
      neighbors testCLoc (LK 4 0) 2 `shouldBe`[ LK 2 0
                                              , LK 2 1
                                              , LK 3 2
                                              , LK 3 0
                                              , LK 3 1
                                              , LK 4 2
                                              , LK 4 0
                                              , LK 4 1
                                              ]

    it "gives the neighbors of a given cell (5)" $
      neighbors testCLoc (LK 2 1) 3 `shouldBe`[ LK 0 2
                                              , LK 0 3
                                              , LK 0 0
                                              , LK 0 1
                                              , LK 1 2
                                              , LK 1 3
                                              , LK 1 0
                                              , LK 1 1
                                              , LK 2 2
                                              , LK 2 3
                                              , LK 2 0
                                              , LK 2 1
                                              , LK 3 2
                                              , LK 3 3
                                              , LK 3 0
                                              , LK 3 1
                                              , LK 4 2
                                              , LK 4 3
                                              , LK 4 0
                                              , LK 4 1
                                              ]

    it "gives the neighbors of a given cell (6)" $
      (S.fromList $ neighbors testCLoc (LK 1 1) 5) `shouldBe` (S.fromList $ neighbors testCLoc (LK 2 1) 5)
