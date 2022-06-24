{- |
   Module      : Picrochole.Data.Plan
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Un plan de bataille pour l'IA.
-}

module Picrochole.Data.Plan
  ( ThreatLevel(..)
  , Objective(..)
  , Plan
  , subordinates
  , objectives
  ) where

import Data.Set ( Set )
import Picrochole.Data.Base
import Picrochole.Data.Board

-- | Niveau de risque associé à une destination.
data ThreatLevel = None
                 | AtRisk
  deriving (Eq, Show)

-- | Un objectif auquel sont associées des unités.
data Objective = Objective { target :: CellKey
                           , assigned :: Set UnitKey
                           , reinforcements :: Set UnitKey
                           }
  deriving Show

-- | Un plan de bataille pour l'IA.
data Plan = Plan { subordinates_ :: Set UnitKey
                 , objectives_ :: [Objective]
                 }
  deriving Show

-- | Renvoie l'ensemble des subordonnés impliqués dans le plan.
subordinates :: Plan -> Set UnitKey
subordinates = subordinates_

-- | Renvoie l'ensemble des objectifs du plan.
objectives :: Plan -> [Objective]
objectives = objectives_
