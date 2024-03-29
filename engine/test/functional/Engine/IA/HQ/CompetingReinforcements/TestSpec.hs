{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Engine.IA.HQ.CompetingReinforcements.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.IA.HQ.CompetingReinforcements.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Config
import qualified Picrochole.Data.Orders as O
import Picrochole.Engine.IA.HQ ( schedule )
import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "send reinforcements if enemies are in sight" $ do
    mpieces <- loadSomeDir "test/functional/Engine/IA/HQ/CompetingReinforcements/"
    mexpected <- O.readOrders "test/functional/Engine/IA/HQ/CompetingReinforcements/expected-orders.json"
    case (mpieces, mexpected) of
      (Left m, _) -> expectationFailure ("unable to parse the test files, got: " ++ m)
      (Right _, Left m) -> expectationFailure ("unable to parse the result file, got: " ++ m)
      (Right pieces, Right expected) -> case (satlas pieces)
                                             &> (sunits pieces)
                                             &> (sreports pieces)
                                             &> (sorders pieces)
                                             &> (splan pieces)
                                             &> (sconfig pieces)
                                             &> (scurrentTurn pieces)
                                        of
        Nothing -> expectationFailure "missing test files"
        Just ((((((atlas, units), reports), orders), plan), config), tcount) -> do
          let res = schedule
                    atlas
                    tcount
                    units
                    (getHQ config (iaFaction config))
                    (iaFaction config)
                    plan
                    reports
                    orders
          (O.toList res) `shouldMatchList` (O.toList expected)
