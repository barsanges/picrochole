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

import qualified Data.Map as M -- FIXME
import Picrochole.Data.Keys

-- | Caractéristiques d'un dépôt de ravitaillement.
data Store = Store { sLocation :: LocationKey
                   , sFaction :: FactionKey
                   }
  deriving Show

-- | L'ensemble des dépôts de ravitaillement de la partie.
newtype CStores = CS (M.Map StoreKey Store)
  deriving Show
