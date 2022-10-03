{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Data.Plan.ParseFail3.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Data.Plan.ParseFail3.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Plan

spec :: Spec
spec = do
  it "fail to parse the plan if reinforcements are also directly assigned to a target" $ do
    mplan <- readPlan "test/Data/Plan/ParseFail3/ia-plan.json"
    case mplan of
      Left m -> m `shouldBe` "the following units are assigned to an\
                             \ objective but belong to the reserve:\
                             \ [UK \"3d Infantry Blue\"]"
      Right _ -> expectationFailure "the file should fail to be parsed as a proper plan"
