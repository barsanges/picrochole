{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Engine.IA.PathFinding.NoPath.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Engine.IA.PathFinding.NoPath.TestSpec ( spec ) where

import qualified Data.Map as M
import Test.Hspec

import Picrochole.Data.Base
import Picrochole.Data.Config
import Picrochole.Engine.IA.PathFinding ( route )
import Picrochole.JSON.Pieces

spec :: Spec
spec = do
  it "return an empty list if an admissible path does not exist" $ do
    mpieces <- loadSomeDir "test/Engine/IA/PathFinding/NoPath/"
    case mpieces of
      Left m -> expectationFailure ("unable to parse the test files, got: " ++ m)
      Right pieces -> case (satlas pieces)
                           &> (sunits pieces)
                           &> (sorders pieces)
                           &> (sconfig pieces)
                      of
        Nothing -> expectationFailure "missing test files"
        Just (((atlas, units), orders), config) -> do
          let res = route atlas [UK "HQ Blue"] 0 units orders (getHQ config)
          res `shouldBe` (M.fromList [(UK "HQ Blue", [])])
