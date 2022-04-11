{- |
   Module      : Picrochole.Engine.Update
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Actualise la date de mise à jour de l'unité.
-}

module Picrochole.Engine.Update
  ( runUpdate
  ) where

import Data.Time ( UTCTime )
import Picrochole.Data.Keys
import Picrochole.Data.Stats
import Picrochole.Data.World

-- | Actualise la date de mise à jour de l'unité.
runUpdate :: UTCTime
          -> UnitKey
          -> World
          -> World
runUpdate t' k w = case lookupUnit k cs of
    Nothing -> w
    Just s -> w { cStats = cs' }
      where
        s' = s { uLastUpdate = t' }
        cs' = insertUnit s' cs
    where
      cs = cStats w
