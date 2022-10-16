{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Data.Plan.ParseFail2.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Data.Plan.ParseFail2.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Plan

spec :: Spec
spec = do
  it "fail to parse the plan if reinforcements do not belong to the reserve" $ do
    mplan <- readPlan "test/functional/Data/Plan/ParseFail2/ia-plan.json"
    case mplan of
      Left m -> m `shouldBe` "the following units are assigned as\
                             \ reinforcements but do not appear in\
                             \ the reserve: [UK \"3d Infantry Blue\"]"
      Right _ -> expectationFailure "the file should fail to be parsed as a proper plan"
