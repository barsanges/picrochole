{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Picrochole.Data.Base
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Types de base pour décrire le jeu.
-}

module Picrochole.Data.Base
  ( TurnCount
  , Faction(..)
  , readFaction
  , opponent
  , UnitKey(..)
  , UnitKind(..)
  , Tile(..)
  , speed
  ) where

import qualified Data.Text as T
import GHC.Generics ( Generic )

-- | Numéro du tour en cours.
type TurnCount = Int

-- | Faction à laquelle appartient une unité.
data Faction = Blue
             | Red
  deriving (Eq, Generic, Show)

-- | Transforme une chaîne de caractères en un identifiant de faction.
readFaction :: T.Text -> Maybe Faction
readFaction txt = case T.toLower (T.strip txt) of
  "blue" -> Just Blue
  "red" -> Just Red
  _ -> Nothing

-- | Renvoie la faction adverse.
opponent :: Faction -> Faction
opponent Blue = Red
opponent Red = Blue

-- | Identifiant unique d'une unité.
newtype UnitKey = UK T.Text
  deriving (Eq, Generic, Ord, Show)

-- | Arme d'une unité.
data UnitKind = Infantery
              | Cavalery
              | Artillery
  deriving (Eq, Generic, Show)

-- | Nature du terrain sur une case.
data Tile = Road
          | Land
          | Water
  deriving (Eq, Generic, Show)

-- | Renvoie la vitesse à laquelle un type d'unité progresse sur un type de
-- case.
speed :: UnitKind -> Tile -> Double
speed Cavalery Road = 2
speed Infantery Road = 1
speed Artillery Road = 1
speed Cavalery Land = 1
speed Infantery Land = 0.5
speed Artillery Land = 0.25
speed _ Water = 0
