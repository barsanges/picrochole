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
  , readPlan
  ) where

import Data.Aeson ( eitherDecodeFileStrict )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import qualified Picrochole.JSON.Plan as J

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
data Plan = Plan { subordinates :: Set UnitKey
                 , objectives :: [Objective]
                 }
  deriving Show

-- | Lit un fichier contenant le plan de bataille de l'IA.
readPlan :: FilePath -> IO (Either String Plan)
readPlan fp = do
  mp <- eitherDecodeFileStrict fp
  case mp of
    Left m -> return (Left m)
    Right p -> return (Right (Plan { subordinates = vectorToSet (fmap UK (J.subordinates p))
                                   , objectives = V.toList (fmap readObjective (J.objectives p))
                                   }))

-- | Crée une instance de `Objective` à partir de paramètres lus dans un JSON.
readObjective :: J.Objective -> Objective
readObjective o = Objective { target = CK (J.target o)
                            , assigned = vectorToSet (fmap UK (J.assigned o))
                            , reinforcements = vectorToSet (fmap UK (J.reinforcements o))
                            }

-- | Transforme un vecteur en un set.
vectorToSet :: Ord a => Vector a -> Set a
vectorToSet = S.fromList . V.toList
