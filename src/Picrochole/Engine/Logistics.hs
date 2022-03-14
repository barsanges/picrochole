{- |
   Module      : Picrochole.Engine.Logistics
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Met à jour les caractéristiques du dépôt de ravitaillement courant.
-}

module Picrochole.Engine.Logistics
  ( runLogistics
  ) where

import Picrochole.Data.Keys
import Picrochole.Data.Store

-- | Met à jour les caractéristiques du dépôt de ravitaillement courant.
runLogistics :: StoreKey
             -> CStores
             -> CStores
runLogistics k ct = ct -- FIXME
