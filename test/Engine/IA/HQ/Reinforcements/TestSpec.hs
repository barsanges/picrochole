{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Engine.IA.HQ.Reinforcements.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.IA.HQ.Reinforcements.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Config
import qualified Picrochole.Data.Orders as O
import Picrochole.Engine.IA.HQ ( schedule )
import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "send reinforcements if an enemy is in sight" $ do
    mpieces <- loadSomeDir "test/Engine/IA/HQ/Reinforcements/"
    mexpected <- O.readOrders "test/Engine/IA/HQ/Reinforcements/expected-orders.json"
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
                    (limit config)
                    plan
                    reports
                    orders
          (O.toList res) `shouldMatchList` (O.toList expected)
