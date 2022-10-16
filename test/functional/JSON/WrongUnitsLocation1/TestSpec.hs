{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JSON.WrongUnitsLocation1.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module JSON.WrongUnitsLocation1.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "fail to parse the files if the location of some units is not in the grid" $ do
    everything <- loadEverythingDir "test/functional/JSON/WrongUnitsLocation1"
    case everything of
      Left m -> m `shouldBe` "some units are not on legitimate locations: [CK 230]"
      Right _ -> expectationFailure "the files should fail to be parsed properly"
