{- |
   Module      : Picrochole.Engine.Logistics
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Met à jour le ravitaillement de l'unité courante.
-}

module Picrochole.Engine.Logistics
  ( runLogistics
  ) where

import Data.Time ( UTCTime, NominalDiffTime )
import qualified Data.Time as T
import Picrochole.Data.Keys
import Picrochole.Data.Layout
import Picrochole.Data.Stats
import Picrochole.Data.Store
import Picrochole.Data.World
import Picrochole.Utils.Time
import qualified Picrochole.Utils.XsMap as Xs

-- | Met à jour le ravitaillement de l'unité courante.
runLogistics :: UTCTime
             -> UnitKey
             -> World
             -> World
runLogistics t' k w = case Xs.lookupKey k cs of
    Nothing -> w
    Just s -> w { cLocations = cl'
                , cStats = cs'
                }
      where
        dt = T.diffUTCTime t' (uLastUpdate s)
        (s', cl') = upSupply dt ct s cl
        cs' = Xs.insertKey k s' cs
    where
      cl = cLocations w
      cs = cStats w
      ct = cStores w
-- FIXME : passer par une structure de type patch pour combiner et appliquer
-- les modifications ?

-- | Actualise le ravitaillement de l'unité.
upSupply :: NominalDiffTime
         -> CStores
         -> Stats
         -> CLocations
         -> (Stats, CLocations)
upSupply dt ct s cl = (s', cl')
  where
    (cl', s_) = restockOnCivilians cl (restockOnStore ct (consumeSupply dt s))
    s' = supplyImpactOnMorale s_

-- | Met à jour le ravitaillement d'une unité en fonction de sa consommation.
consumeSupply :: NominalDiffTime -> Stats -> Stats
consumeSupply dt s = s { uSupply = supply' }
  where
    decr = (uSupplyConsumption s) * (toSeconds dt)
    supply' = max ((uSupply s) - decr) 0

-- | Ravitaille l'unité à partir d'un dépôt.
restockOnStore :: CStores -> Stats -> Stats
restockOnStore ct s = case nearestAlliedStore ct fk lk of
  Just _ -> s { uSupply = 1 }
  Nothing ->  s
  where
    fk = uFaction s
    lk = uLocation s

-- | Ravitaille l'unité via des réquisitions.
restockOnCivilians :: CLocations -> Stats -> (CLocations, Stats)
restockOnCivilians cl s = if uRequisition s
                          then (cl', s')
                          else (cl, s)
  where
    lk = uLocation s
    needed = 1 - (uSupply s)
    ngbIdx = neighbors cl lk (uRequisitionRadius s)
    ngb = lLookupSeveral cl ngbIdx
    availableSupply = foldr (\ x t -> lCivilianSupply x + t) 0 ngb
    rate = min (needed / availableSupply) 1
    ngb' = fmap adjust ngb
    cl' = bulkUpdate cl (zip ngbIdx ngb')
    s' = s { uSupply = (uSupply s) + rate * availableSupply }
    adjust :: Location -> Location
    adjust x = x { lCivilianSupply = (1 - rate) * (lCivilianSupply x) }

-- | Met à jour le moral en fonction du niveau de ravitaillement.
supplyImpactOnMorale :: Stats -> Stats
supplyImpactOnMorale s = s { uMorale = m' }
  where
    dm = if (uSupply s) <= 0
         then -(uSupplyImpactOnMorale s)
         else 0
    m' = (uMorale s) + dm
