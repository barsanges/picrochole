{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JSON.WrongUnitsLocation2.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module JSON.WrongUnitsLocation2.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "fail to parse the files if some units cannot be placed at their location" $ do
    everything <- loadEverythingDir "test/functional/JSON/WrongUnitsLocation2"
    case everything of
      Left m -> m `shouldBe` "some cells have more units than allowed: [CK 16]"
      Right _ -> expectationFailure "the files should fail to be parsed properly"
