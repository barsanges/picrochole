{- |
   Module      : Picrochole.Data.Store
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour les dépôts de ravitaillement.
-}

module Picrochole.Data.Store
  ( Store(..)
  , CStores
  ) where

import Picrochole.Data.Keys
import Picrochole.Utils.XMap

-- | Caractéristiques d'un dépôt de ravitaillement.
data Store = Store { sLocation :: LocationKey
                   , sFaction :: FactionKey
                   }
  deriving Show

instance HasLocation Store where
  getLocation = sLocation

-- | L'ensemble des dépôts de ravitaillement de la partie.
type CStores = XMap StoreKey Store
