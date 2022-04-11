{- |
   Module      : Picrochole.Data.Stats
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour l'état des unités.
-}

module Picrochole.Data.Stats
  ( Stats(..)
  , CStats
  , lookupUnit
  , insertUnit
  , hasEnnemies
  , getEnnemies
  ) where

import Data.Time ( UTCTime )
import Picrochole.Data.Keys
import Picrochole.Utils.XsMap

-- | Caractéristiques d'une unité.
data Stats = Stats { uLocation :: LocationKey
                   , uLastUpdate :: UTCTime
                   , uFaction :: FactionKey
                   , uMorale :: Double
                   , uMovementImpactOnVigor :: Double
                   , uRequisition :: Bool
                   , uRequisitionRadius :: Int
                   , uSpeed :: Double
                   , uStillImpactOnVigor :: Double
                   , uSupply :: Double
                   , uSupplyConsumption :: Double
                   , uSupplyImpactOnMorale :: Double
                   , uUnitKey :: UnitKey
                   , uVigor :: Double
                   }
  deriving Show
-- FIXME : distinguer les paramètres variables et ceux qui ne le sont pas
-- (par exemple en intégrant ces derniers dans un type pour lequel on ne
-- donne que des getters) ?
-- FIXME : créer un type spécifique pour les variables comprises entre
-- 0 et 1 ?

instance HasLocation Stats where
  getLocation = uLocation

-- | L'ensemble des unités de la partie.
newtype CStats = CS (XsMap UnitKey Stats)
  deriving Show

-- | Renvoie l'unité associée à la clef donnée.
lookupUnit :: UnitKey -> CStats -> Maybe Stats
lookupUnit k (CS xs) = lookupKey k xs

-- | Insère l'unité dans le conteneur.
insertUnit :: Stats -> CStats -> CStats
insertUnit x (CS xs) = CS (insertKey (uUnitKey x) x xs)

-- | Indique si un lieu contient des unités n'appartenant pas à la faction
-- donnée.
hasEnnemies :: LocationKey -> FactionKey -> CStats -> Bool
hasEnnemies lk f cs = null (getEnnemies lk f cs)

-- | Renvoie toutes les unités situées en un lieu donné et n'appartenant
-- *pas* à la faction donnée.
getEnnemies :: LocationKey -> FactionKey -> CStats -> [Stats]
getEnnemies lk f (CS xs) = filter (\ s -> f /= uFaction s) (lookupLocation lk xs)
