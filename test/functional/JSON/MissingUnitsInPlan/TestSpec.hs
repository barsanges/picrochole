{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JSON.MissingUnitsInPlan.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module JSON.MissingUnitsInPlan.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "fail to parse the files if all IA units are not listed in the plan" $ do
    everything <- loadEverythingDir "test/functional/JSON/MissingUnitsInPlan"
    case everything of
      Left m -> m `shouldBe` "some IA units are not listed in the plan file: [UK \"HQ Blue\"]"
      Right _ -> expectationFailure "the files should fail to be parsed properly"
