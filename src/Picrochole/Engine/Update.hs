{- |
   Module      : Picrochole.Engine.Update
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Met à jour les statistiques de l'unité courante.
-}

module Picrochole.Engine.Update
  ( runUpdate
  ) where

import Data.Time ( UTCTime )
import Picrochole.Data.Keys
import Picrochole.Data.Stats
import qualified Picrochole.Utils.XsMap as Xs

-- | Met à jour les statistiques de l'unité courante.
runUpdate :: UTCTime
          -> UnitKey
          -> CStats
          -> CStats
runUpdate t' k cs = case Xs.lookupKey k cs of
    Nothing -> cs
    Just s -> Xs.insertKey k s' cs
      where
        s' = s { uLastUpdate = t' }
