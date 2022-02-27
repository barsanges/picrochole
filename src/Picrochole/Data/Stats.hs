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

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M -- FIXME : IntMap ?
import Picrochole.Data.Keys

-- | Caractéristiques d'une unité.
data Stats = Stats { uLocation :: NE.NonEmpty LocationKey
                   }

-- | L'ensemble des unités de la partie.
newtype CStats = CS (M.Map UnitKey Stats)
