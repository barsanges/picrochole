{- |
   Module      : Picrochole.Data.Initiative
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Ordre d'initiative des unités.
-}

module Picrochole.Data.Initiative
  ( Initiative
  , readInitiative
  ) where

import Data.Aeson ( eitherDecodeFileStrict )
import qualified Data.Vector as V

import Picrochole.Data.Base

-- | Ordre d'initiative des unités.
type Initiative = [UnitKey]

-- | Construit une instance de `[UnitKey]` à partir d'un fichier JSON.
readInitiative :: FilePath -> IO (Either String [UnitKey])
readInitiative fp = do
  mi <- eitherDecodeFileStrict fp
  case mi of
    Left m -> return (Left m)
    Right i -> return (Right (V.toList (fmap UK i)))
