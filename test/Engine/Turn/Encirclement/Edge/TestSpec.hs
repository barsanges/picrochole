{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Engine.Turn.Encirclement.Edge.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.Turn.Encirclement.Edge.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Units
import qualified Picrochole.Data.Units as U
import Picrochole.Engine.Turn ( encirclement )
import Picrochole.JSON.Pieces

instance Eq Unit where
  x == y = x `approxEq` y

spec :: Spec
spec = do
  it "units may be surrounded even if they are on the edge of the map" $ do
    mpieces <- loadSomeDir "test/Engine/Turn/Encirclement/Edge/"
    mexpected <- readUnits "test/Engine/Turn/Encirclement/Edge/expected-units.json"
    case (mpieces, mexpected) of
      (Left m, _) -> expectationFailure ("unable to parse the test files, got: " ++ m)
      (Right _, Left m) -> expectationFailure ("unable to parse the result file, got: " ++ m)
      (Right pieces, Right expected) -> case (satlas pieces)
                                             &> (sunits pieces)
                                        of
        Nothing -> expectationFailure "missing test files"
        Just (atlas, units) -> do
          let res = encirclement
                    atlas
                    units
          (U.toList res) `shouldMatchList` (U.toList expected)
