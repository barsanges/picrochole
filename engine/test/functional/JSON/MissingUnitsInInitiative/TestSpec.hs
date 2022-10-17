{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JSON.MissingUnitsInInitiative.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module JSON.MissingUnitsInInitiative.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "fail to parse the files if all units are not listed in the initiative file" $ do
    everything <- loadEverythingDir "test/functional/JSON/MissingUnitsInInitiative"
    case everything of
      Left m -> m `shouldBe` "some units are not listed in the initiative file: [UK \"1st Red\"]"
      Right _ -> expectationFailure "the files should fail to be parsed properly"
