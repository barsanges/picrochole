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
  , location'
  , position'
  , locations
  , decrStrength
  , setPosition
  , removeFaction
  , getDist
  , capacityLeft
  , hasUnit
  , isContested
  ) where

import Data.Set ( Set )
import qualified Data.Set as S

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

-- | Renvoie l'identifiant de la cellule sur laquelle se trouve une unité du
-- plateau de jeu.
location' :: Units -> UnitKey -> Maybe CellKey
location' xs ukey = fmap location (lookupKey ukey xs)

-- | Renvoie la position d'une unité sur le plateau de jeu.
position' :: Units -> UnitKey -> Maybe Position
position' xs ukey = fmap position (lookupKey ukey xs)

-- | Renvoie l'emplacement de toutes les unités d'un camp.
locations :: Units -> Faction -> Set CellKey
locations xs f = foldr go S.empty xs
  where
    go :: Unit -> Set CellKey -> Set CellKey
    go u s = if faction u == f
             then S.insert (location u) s
             else s

-- | Diminue la force d'une unité sur le plateau de jeu.
decrStrength :: UnitKey -> Double -> Units -> Units
decrStrength uk ds xs = case lookupKey uk xs of
  Nothing -> xs
  Just u -> if s' > 0
            then insertKey uk u' xs
            else deleteKey uk xs
    where
      s' = (strength u) - ds
      u' = u { strength_ = s' }

-- | Supprime toutes les unités d'un camp sur la case donnée.
removeFaction :: Faction -> CellKey -> Units -> Units
removeFaction f ck xs = foldr go xs bunits
  where
    bunits = lookupLocationContent ck xs

    go :: Unit -> Units -> Units
    go u ys = if faction u == f
              then deleteKey (unitKey u) ys
              else ys

-- | Change la position d'une unité sur le plateau de jeu.
setPosition :: UnitKey -> Position -> Units -> Units
setPosition ukey pos xs = case lookupKey ukey xs of
  Nothing -> xs
  Just u -> insertKey ukey u' xs
    where
      u' = u { position_ = pos }

-- | Renvoie la distance à vol d'oiseau entre deux unités.
getDist :: Atlas -> Units -> UnitKey -> UnitKey -> Maybe Int
getDist atlas xs u v = do
  u' <- lookupKey u xs
  v' <- lookupKey v xs
  return (dist (gridSize atlas) (location u') (location v'))

-- | Renvoie la capacité d'accueil restante de la case donnée.
capacityLeft :: Atlas -> Units -> Faction -> CellKey -> Double
capacityLeft atlas xs f ck = foldr go maxCapacity bunits
  where
    maxCapacity = capacity (getHex atlas ck)
    bunits = lookupLocationContent ck xs

    go :: Unit -> Double -> Double
    go u c = if faction u == f
             then c - (strength u)
             else c

-- | Indique si la case donnée contient une unité du camp indiqué.
hasUnit :: Units -> Faction -> CellKey -> Bool
hasUnit xs f ckey = any go (lookupLocationContent ckey xs)
  where
    go :: Unit -> Bool
    go u = (faction u) == f

-- | Indique si la case donnée contient des unités des deux camps.
isContested :: Units -> CellKey -> Bool
isContested xs ckey = (hasUnit xs Blue ckey) && (hasUnit xs Red ckey)
