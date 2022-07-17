{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Picrochole.Data.Plan
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Un plan de bataille pour l'IA.
-}

module Picrochole.Data.Plan
  ( ThreatLevel(..)
  , Objective(..)
  , Plan(..)
  ) where

import GHC.Generics ( Generic )
import Data.Set ( Set )
import Picrochole.Data.Base
import Picrochole.Data.Board

-- | Niveau de risque associé à une destination.
data ThreatLevel = None
                 | AtRisk
  deriving (Eq, Generic, Show)

-- | Un objectif auquel sont associées des unités.
data Objective = Objective { target :: CellKey
                           , assigned :: Set UnitKey
                           , reinforcements :: Set UnitKey
                           }
  deriving Show

-- | Un plan de bataille pour l'IA.
data Plan = Plan { subordinates :: Set UnitKey
                 , objectives :: [Objective]
                 }
  deriving Show
