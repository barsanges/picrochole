{- |
   Module      : Picrochole.Data.World
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Ensemble des conteneurs constituant le monde du jeu.
-}

module Picrochole.Data.World
  ( World(..)
  ) where

import Picrochole.Data.Action
import Picrochole.Data.Layout
import Picrochole.Data.Stats
import Picrochole.Data.Store

-- | Ensemble des conteneurs constituant le monde du jeu.
data World = World { cActions :: CActions
                   , cLocations :: CLocations
                   , cStats :: CStats
                   , cStores :: CStores
                   }
  deriving Show
