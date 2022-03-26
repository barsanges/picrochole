{- |
   Module      : Picrochole.Engine.Update
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Met à jour les statistiques de l'unité courante.
-}

module Picrochole.Engine.Update
  ( runUpdate
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

-- | Met à jour les statistiques de l'unité courante.
runUpdate :: UTCTime
          -> UnitKey
          -> World
          -> World
runUpdate t' k w = case Xs.lookupKey k cs of
    Nothing -> w
    Just s -> w { cLocations = cl'
                , cStats = cs'
                }
      where
        dt = T.diffUTCTime t' (uLastUpdate s)
        (s', cl') = upSupply dt ct (upLastUpdate t' s) cl
        cs' = Xs.insertKey k s' cs
    where
      cl = cLocations w
      cs = cStats w
      ct = cStores w
-- FIXME : passer par une structure de type patch pour combiner et appliquer
-- les modifications ?

-- | Actualise la date de mise à jour de l'unité.
upLastUpdate :: UTCTime -> Stats -> Stats
upLastUpdate t' s = s { uLastUpdate = t' }

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
    supply' = (uSupply s) - decr

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
    m' = (uMorale s) - ((uSupply s) * (uSupplyImpactOnMorale s))
