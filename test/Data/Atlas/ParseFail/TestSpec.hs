{- |
   Module      : Data.Atlas.ParseFail.TestSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Data.Atlas.ParseFail.TestSpec ( spec ) where

import Test.Hspec

import Picrochole.Data.Atlas

spec :: Spec
spec = do
  it "fail to parse the atlas if the dimensions do not match" $ shouldFail "test/Data/Atlas/ParseFail/atlas.json"
  where
    shouldFail :: FilePath -> Expectation
    shouldFail fp = do
      ia <- readAtlas fp
      case ia of
        Left m -> m `shouldBe` "unable to parse the atlas, due to a mismatch between the grid size and the length of the vector"
        Right _ -> expectationFailure "the file should fail to be parsed as a proper atlas"
