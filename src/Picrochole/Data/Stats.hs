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

import Data.Time ( UTCTime )
import Picrochole.Data.Keys
import Picrochole.Utils.XsMap

-- | Caractéristiques d'une unité.
data Stats = Stats { uLocation :: LocationKey
                   , uLastUpdate :: UTCTime
                   , uFaction :: FactionKey
                   , uMorale :: Double
                   , uRequisition :: Bool
                   , uRequisitionRadius :: Int
                   , uSupply :: Double
                   , uSupplyConsumption :: Double
                   , uSupplyImpactOnMorale :: Double
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
type CStats = XsMap UnitKey Stats
