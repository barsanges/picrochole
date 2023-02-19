{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Data.Structs.RegisterSpec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3
-}

module Data.Structs.RegisterSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Vector as V

import Picrochole.Data.Base
import Picrochole.Data.Structs.Register

instance Arbitrary Header where
  arbitrary = do
    f <- arbitrary
    t <- arbitrary
    s <- arbitrary
    r <- arbitrary
    return Header { from = UK (T.pack f)
                  , to = UK (T.pack t)
                  , sent = s
                  , received = r
                  }

instance Arbitrary a => Arbitrary (Msg a) where
  arbitrary = do
    h <- arbitrary
    c <- arbitrary
    return Msg { header = h
               , content = c
               }

preserveOrder :: [Msg Int] -> Bool
preserveOrder xs = (toList . fromVector . V.fromList $ xs) == xs

spec :: Spec
spec = do
  describe "A register is a structure to store messages between units" $ do
    it "it should always preserve the order of the elements" $ property
      preserveOrder
