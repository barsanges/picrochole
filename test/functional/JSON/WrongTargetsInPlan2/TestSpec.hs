{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JSON.WrongTargetsInPlan2.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module JSON.WrongTargetsInPlan2.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "fail to parse the files if IA targets are not in the atlas" $ do
    everything <- loadEverythingDir "test/functional/JSON/WrongTargetsInPlan2"
    case everything of
      Left m -> m `shouldBe` "some targets of the IA are not legitimate locations: [CK 230,CK 160]"
      Right _ -> expectationFailure "the files should fail to be parsed properly"
