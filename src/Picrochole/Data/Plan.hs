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
  , objectives
  , concentration
  , reserve
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
data Plan = Plan { objectives_ :: [Objective]
                 , concentration_ :: CellKey
                 , reserve_ :: Set UnitKey
                 }
  deriving Show

-- | Renvoie la liste des objectifs associés à un plan.
objectives :: Plan -> [Objective]
objectives = objectives_

-- | Renvoie le lieu de rassemblement de la réserve.
concentration :: Plan -> CellKey
concentration = concentration_

-- | Renvoie les identifiants des unités qui constituent la réserve.
reserve :: Plan -> Set UnitKey
reserve = reserve_

-- | Lit un fichier contenant le plan de bataille de l'IA.
readPlan :: FilePath -> IO (Either String Plan)
readPlan fp = do
  mp <- eitherDecodeFileStrict fp
  case mp of
    Left m -> return (Left m)
    Right p -> if not (S.null multipleObj)
      then return (Left ("the following units are assigned to multiple\
                         \ objectives: " ++ (show $ S.toList multipleObj)))
      else if not (S.null mixed)
      then return (Left ("the following units are assigned to an objective\
                         \ but belong to the reserve: "
                         ++ (show $ S.toList mixed)))
      else if not (S.null notInRes)
      then return (Left ("the following units are assigned as reinforcements\
                         \ but do not appear in the reserve: "
                         ++ (show $ S.toList notInRes)))
      else return (Right (Plan { objectives_ = obj
                               , concentration_ = CK (J.concentration p)
                               , reserve_ = res
                               }))
      where
        obj = V.toList (fmap readObjective (J.objectives p))
        res = vectorToSet (fmap UK (J.reserve p))
        multipleObj = multiple (fmap assigned obj)
        notInRes = S.difference (S.unions (fmap reinforcements obj)) res
        mixed = S.intersection (S.unions (fmap assigned obj)) res

-- | Renvoie les éléments qui se trouvent dans au moins deux ensembles du
-- groupe d'ensemble `xs`.
multiple :: (Ord a, Foldable t) => t (Set a) -> Set a
multiple xss = snd (foldr go (S.empty, S.empty) xss)
  where
    go xs (done, mul) = (S.union xs done, S.union mul (S.intersection xs done))

-- | Crée une instance de `Objective` à partir de paramètres lus dans un JSON.
readObjective :: J.Objective -> Objective
readObjective o = Objective { target = CK (J.target o)
                            , assigned = vectorToSet (fmap UK (J.assigned o))
                            , reinforcements = vectorToSet (fmap UK (J.reinforcements o))
                            }

-- | Transforme un vecteur en un set.
vectorToSet :: Ord a => Vector a -> Set a
vectorToSet = S.fromList . V.toList
