{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Data.Plan.ParseFail1.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Data.Plan.ParseFail1.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Plan

spec :: Spec
spec = do
  it "fail to parse the plan if units are assigned to multiple objectives" $ do
    mplan <- readPlan "test/functional/Data/Plan/ParseFail1/ia-plan.json"
    case mplan of
      Left m -> m `shouldBe` "the following units are assigned to multiple\
                             \ objectives: [UK \"1st Infantry Blue\"]"
      Right _ -> expectationFailure "the file should fail to be parsed as a proper plan"
