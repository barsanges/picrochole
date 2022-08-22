{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Config
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON des paramètres généraux de la partie.
-}

module Picrochole.JSON.Config
  ( Config(..)
  ) where

import Data.Aeson
import qualified Data.Text as T

-- | Paramètres généraux de la partie.
data Config = Config { iaFaction :: T.Text
                     , hqBlue :: T.Text
                     , hqRed :: T.Text
                     , limit :: Int
                     }
  deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config" go
    where
      go v = do
        ia <- v .: "ia-faction"
        hqb <- v .: "hq-blue"
        hqr <- v .: "hq-red"
        lim <- v .: "limit"
        return Config { iaFaction = ia
                      , hqBlue = hqb
                      , hqRed = hqr
                      , limit = lim
                      }
