{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JSON.WrongTargetsInPlan1.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module JSON.WrongTargetsInPlan1.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "fail to parse the files if IA targets have no capacity" $ do
    everything <- loadEverythingDir "test/functional/JSON/WrongTargetsInPlan1"
    case everything of
      Left m -> m `shouldBe` "some targets of the IA have no capacity: [CK 21,CK 16]"
      Right _ -> expectationFailure "the files should fail to be parsed properly"
