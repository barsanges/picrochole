{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Engine.Turn.Movement.EraseMarker.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.Turn.Movement.EraseMarker.TestSpec ( spec ) where

import Test.Hspec
import qualified Data.Map as M

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Units
import qualified Picrochole.Data.Units as U
import Picrochole.Engine.Turn ( movement )
import Picrochole.JSON.Pieces

instance Eq Unit where
  x == y = x `U.approxEq` y

spec :: Spec
spec = do
  it "markers are erased as soon as a unit enters the cell" $ do
    mpieces <- loadSomeDir "test/functional/Engine/Turn/Movement/EraseMarker/"
    mexpected <- readUnits "test/functional/Engine/Turn/Movement/EraseMarker/expected-units.json"
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
                    (M.fromList [ (UK "HQ Blue", [CK 10, CK 17])
                                , (UK "HQ Red", [CK 36, CK 30])
                                ])
                    units
          (U.toList res) `shouldMatchList` (U.toList expected)
