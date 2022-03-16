{- |
   Module      : Picrochole.Data.Stats
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour l'état des unités.
-}

module Picrochole.Data.Stats
  ( Stats(..)
  , CStats
  ) where

import Data.Time ( UTCTime )
import Picrochole.Data.Keys
import Picrochole.Utils.XsMap

-- | Caractéristiques d'une unité.
data Stats = Stats { uLocation :: LocationKey
                   , uLastUpdate :: UTCTime
                   , uFaction :: FactionKey
                   }
  deriving Show

instance HasLocation Stats where
  getLocation = uLocation

-- | L'ensemble des unités de la partie.
type CStats = XsMap UnitKey Stats
