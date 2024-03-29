{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Engine.Reporting.RecentlyDone.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.Reporting.RecentlyDone.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Config
import qualified Picrochole.Data.Reports as R
import Picrochole.Data.Units
import qualified Picrochole.Data.Units as U
import Picrochole.Engine.Reporting ( reporting )
import Picrochole.JSON.Pieces

instance Eq Unit where
  x == y = x `U.approxEq` y

spec :: Spec
spec = do
  it "don't send a report each turn if the unit is not really close to its HQ" $ do
    mpieces <- loadSomeDir "test/functional/Engine/Reporting/RecentlyDone/"
    mexpected <- R.readReports "test/functional/Engine/Reporting/RecentlyDone/expected-reports.json"
    case (mpieces, mexpected) of
      (Left m, _) -> expectationFailure ("unable to parse the test files, got: " ++ m)
      (Right _, Left m) -> expectationFailure ("unable to parse the result file, got: " ++ m)
      (Right pieces, Right expected) -> case (satlas pieces)
                                             &> (sinitiative pieces)
                                             &> (sunits pieces)
                                             &> (sreports pieces)
                                             &> (sconfig pieces)
                                             &> (scurrentTurn pieces)
                                        of
        Nothing -> expectationFailure "missing test files"
        Just (((((atlas, initiative), units), reports), config), tcount) -> do
          let res = reporting
                    atlas
                    initiative
                    tcount
                    (getHQ config)
                    units
                    reports
          (R.toList res) `shouldMatchList` (R.toList expected)
