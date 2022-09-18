{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Engine.Turn.Movement.CapacityConstraints.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.Turn.Movement.CapacityConstraints.TestSpec ( spec ) where

import Test.Hspec
import qualified Data.Map as M

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Units
import qualified Picrochole.Data.Units as U
import Picrochole.Engine.Turn ( movement )
import Picrochole.JSON.Pieces

instance Eq Unit where
  x == y = x `approxEq` y

spec :: Spec
spec = do
  it "a unit may not advance if there is no room ahead" $ do
    mpieces <- loadSomeDir "test/Engine/Turn/Movement/CapacityConstraints/"
    mexpected <- readUnits "test/Engine/Turn/Movement/CapacityConstraints/expected-units.json"
    case (mpieces, mexpected) of
      (Left m, _) -> expectationFailure ("unable to parse the test files, got: " ++ m)
      (Right _, Left m) -> expectationFailure ("unable to parse the result file, got: " ++ m)
      (Right pieces, Right expected) -> case (satlas pieces)
                                             &> (sinitiative pieces)
                                             &> (sunits pieces)
                                        of
        Nothing -> expectationFailure "missing test files"
        Just ((atlas, initiative), units) -> do
          let res = movement
                    atlas
                    initiative
                    (M.fromList [ (UK "1st Blue", [CK 8, CK 9])
                                , (UK "HQ Blue", [])
                                , (UK "HQ Red", [])
                                ])
                    units
          (U.toList res) `shouldMatchList` (U.toList expected)
