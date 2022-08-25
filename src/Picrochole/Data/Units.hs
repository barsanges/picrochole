{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.Data.Units
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les unités des deux camps.
-}

module Picrochole.Data.Units
  ( Units
  , Unit
  , Position(..)
  , module Picrochole.Data.Structs.XsMap
  , unitKey
  , faction
  , kind
  , strength
  , position
  , location
  , progress
  , mkUnit
  , decrStrength'
  , setPosition'
  ) where

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Structs.XsMap

-- | Une unité du plateau de jeu.
data Unit = Unit { unitKey_ :: UnitKey
                 , faction_ :: Faction
                 , kind_ :: UnitKind
                 , strength_ :: Double
                 , position_ :: Position
                 }
  deriving Show

-- | Position d'une unité sur le plateau de jeu.
data Position = Position { currentCell :: CellKey
                         , currentProgress :: Maybe Double
                         }
  deriving Show

-- | Les unités des deux camps.
type Units = XsMap CellKey UnitKey Faction Unit

-- | Renvoie l'identifiant de l'unité.
unitKey :: Unit -> UnitKey
unitKey = unitKey_

-- | Renvoie la faction de l'unité.
faction :: Unit -> Faction
faction = faction_

-- | Renvoie l'arme de l'unité.
kind :: Unit -> UnitKind
kind = kind_

-- | Renvoie la force de l'unité.
strength :: Unit -> Double
strength = strength_

-- | Renvoie la position de l'unité.
position :: Unit -> Position
position = position_

-- | Renvoie l'identifiant de la cellule sur laquelle se trouve l'unité.
location :: Unit -> CellKey
location = currentCell . position_

-- | Renvoie l'état d'avancement de l'unité sur sa case.
progress :: Unit -> Maybe Double
progress = currentProgress . position_

-- | Renvoie une unité. FIXME : à supprimer.
mkUnit :: UnitKey
       -> Faction
       -> UnitKind
       -> Double
       -> CellKey
       -> Maybe Double
       -> Unit
mkUnit ky f ki s l p = Unit { unitKey_ = ky
                            , faction_ = f
                            , kind_ = ki
                            , strength_ = s
                            , position_ = Position { currentCell = l
                                                   , currentProgress = p
                                                   }
                            }

-- | Diminue la force d'une unité sur le plateau de jeu.
decrStrength' :: UnitKey -> Double -> Units -> Units
decrStrength' uk ds xs = case lookupKey uk xs of
  Nothing -> xs
  Just u -> if s' > 0
            then insertKey uk u' xs
            else deleteKey uk xs
    where
      s' = (strength u) - ds
      u' = u { strength_ = s' }

-- | Change la position d'une unité sur le plateau de jeu.
setPosition' :: UnitKey -> Position -> Units -> Units
setPosition' ukey pos xs = case lookupKey ukey xs of
  Nothing -> xs
  Just u -> insertKey ukey u' xs
    where
      u' = u { position_ = pos }
