{- |
   Module      : Picrochole.Data.Store
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour les dépôts de ravitaillement.
-}

module Picrochole.Data.Store
  ( Store(..)
  , CStores
  , nearestAlliedStore
  ) where

import Picrochole.Data.Keys
import Picrochole.Data.Layout
import Picrochole.Utils.XMap

-- | Caractéristiques d'un dépôt de ravitaillement.
data Store = Store { sLocation :: LocationKey
                   , sFaction :: FactionKey
                   , sRadius :: Int
                   }
  deriving Show

instance HasLocation Store where
  getLocation = sLocation

-- | L'ensemble des dépôts de ravitaillement de la partie.
type CStores = XMap StoreKey Store

-- | Renvoie le dépôt allié le plus proche, sous réserve que l'emplacement
-- de départ soit dans le rayon d'action du dépôt.
nearestAlliedStore :: CStores -> FactionKey -> LocationKey -> Maybe Store
nearestAlliedStore ct fk lk = foldr go Nothing ct
  where
    go :: Store -> Maybe Store -> Maybe Store
    go s ms = if ((sFaction s) == fk) && d1 < (sRadius s)
              then case ms of
                     Nothing -> Just s
                     Just u -> if d1 < d2
                               then Just s
                               else ms
                       where
                         d2 = dist (sLocation u) lk
              else ms
      where
        d1 = dist (sLocation s) lk
