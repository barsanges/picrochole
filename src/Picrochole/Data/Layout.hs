{- |
   Module      : Picrochole.Data.Layout
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour la disposition du terrain.
-}

module Picrochole.Data.Layout
  ( Ground(..)
  , Location(..)
  , CLocations
  ) where

import qualified Data.Vector as V
import Picrochole.Data.Keys

-- | Type de terrain associé à une case de la carte.
data Ground = Plain
            | Forest
            | Road
            | City
            | River
            | Bridge
  deriving (Show, Eq)

-- | Une case de la carte.
data Location = Loc { ground :: Ground
                    , height :: Int
                    }
  deriving Show

-- | La carte, i.e. l'ensemble des cases de la carte.
newtype CLocations = CL (V.Vector Location)
  deriving Show
