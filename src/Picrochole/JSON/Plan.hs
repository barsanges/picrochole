{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Plan
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON du plan de bataille de l'IA.
-}

module Picrochole.JSON.Plan
  ( Plan(..)
  , Objective(..)
  ) where

import Data.Aeson
import Data.Text ( Text )
import Data.Vector ( Vector )

-- | Un objectif auquel sont associées des unités.
data Objective = Objective { target :: Int
                           , assigned :: Vector Text
                           , reinforcements :: Vector Text
                           }
  deriving Show

-- | Un plan de bataille pour l'IA.
data Plan = Plan { subordinates :: Vector Text
                 , objectives :: Vector Objective
                 }
  deriving Show

instance FromJSON Objective where
  parseJSON = withObject "Objective" go
    where
      go v = do
        t <- v .: "target"
        a <- v .: "assigned"
        r <- v .: "reinforcements"
        return Objective { target = t
                         , assigned = a
                         , reinforcements = r
                         }

instance FromJSON Plan where
  parseJSON = withObject "Plan" go
    where
      go v = do
        s <- v .: "subordinates"
        o <- v .: "objectives"
        return Plan { subordinates = s
                    , objectives = o
                    }
