{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Engine.Turn.Bombing.OutOfRange.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.Turn.Bombing.OutOfRange.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Units
import qualified Picrochole.Data.Units as U
import Picrochole.Engine.Turn ( bombing )
import Picrochole.JSON.Pieces

instance Eq Unit where
  x == y = x `U.approxEq` y

spec :: Spec
spec = do
  it "artillery has a range of only 1 hex" $ do
    mpieces <- loadSomeDir "test/functional/Engine/Turn/Bombing/OutOfRange/"
    mexpected <- readUnits "test/functional/Engine/Turn/Bombing/OutOfRange/expected-units.json"
    case (mpieces, mexpected) of
      (Left m, _) -> expectationFailure ("unable to parse the test files, got: " ++ m)
      (Right _, Left m) -> expectationFailure ("unable to parse the result file, got: " ++ m)
      (Right pieces, Right expected) -> case (satlas pieces)
                                             &> (sinitiative pieces)
                                             &> (sunits pieces)
                                        of
        Nothing -> expectationFailure "missing test files"
        Just ((atlas, initiative), units) -> do
          let res = bombing
                    atlas
                    initiative
                    units
          (U.toList res) `shouldMatchList` (U.toList expected)
