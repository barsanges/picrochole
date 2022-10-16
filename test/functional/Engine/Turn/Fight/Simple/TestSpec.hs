{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Engine.Turn.Fight.Simple.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.Turn.Fight.Simple.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Units
import qualified Picrochole.Data.Units as U
import Picrochole.Engine.Turn ( fight )
import Picrochole.JSON.Pieces

instance Eq Unit where
  x == y = x `U.approxEq` y

spec :: Spec
spec = do
  it "a fight involves all non artillery units on the same cell" $ do
    mpieces <- loadSomeDir "test/functional/Engine/Turn/Fight/Simple/"
    mexpected <- readUnits "test/functional/Engine/Turn/Fight/Simple/expected-units.json"
    case (mpieces, mexpected) of
      (Left m, _) -> expectationFailure ("unable to parse the test files, got: " ++ m)
      (Right _, Left m) -> expectationFailure ("unable to parse the result file, got: " ++ m)
      (Right pieces, Right expected) -> case (satlas pieces)
                                             &> (sunits pieces)
                                        of
        Nothing -> expectationFailure "missing test files"
        Just (atlas, units) -> do
          let res = fight
                    atlas
                    units
          (U.toList res) `shouldMatchList` (U.toList expected)
