{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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

import GHC.Generics ( Generic )
import Data.Aeson
import Data.Set ( Set )
import qualified Data.Set as S
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

-- | Sérialisation.

instance ToJSON ThreatLevel
instance FromJSON ThreatLevel

instance ToJSON Objective where
  -- toJSON :: Objective -> Value
  toJSON obj = object [ "target" .= target obj
                      , "assigned" .= S.toList (assigned obj)
                      , "reinforcements" .= S.toList (reinforcements obj)
                      ]

instance FromJSON Objective where
  -- parseJSON :: Objective -> Value
  parseJSON = withObject "Objective" go
    where
      -- go :: Object -> Parser Plan
      go v = do
        t <- v .: "target"
        a <- v .: "assigned"
        r <- v .: "reinforcements"
        return Objective { target = t
                         , assigned = S.fromList a
                         , reinforcements = S.fromList r
                         }

instance ToJSON Plan where
  -- toJSON :: Plan -> Value
  toJSON plan = object [ "subordinates" .= S.toList (subordinates_ plan)
                       , "objectives" .= objectives_ plan
                       ]

instance FromJSON Plan where
  -- parseJSON :: Value -> Parser Plan
  parseJSON = withObject "Plan" go
    where
      -- go :: Object -> Parser Plan
      go v = do
        s <- v .: "subordinates"
        o <- v .: "objectives"
        return Plan { subordinates_ = S.fromList s
                    , objectives_ = o
                    }
