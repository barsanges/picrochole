{- |
   Module      : Picrochole.Data.Base
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Types de base pour décrire le jeu.
-}

module Picrochole.Data.Base
  ( TurnCount
  , Faction(..)
  , opponent
  , UnitKey
  , UnitKind(..)
  , Tile(..)
  ) where

-- | Numéro du tour en cours.
type TurnCount = Int

-- | Faction à laquelle appartient une unité.
data Faction = Blue
             | Red
  deriving (Eq, Show)

-- | Renvoie la faction adverse.
opponent :: Faction -> Faction
opponent Blue = Red
opponent Red = Blue

-- | Identifiant unique d'une unité.
newtype UnitKey = UK Int
  deriving (Eq, Ord, Show)

-- | Arme d'une unité.
data UnitKind = Infantery
              | Cavalery
              | Artillery
  deriving (Eq, Show)

-- | Nature du terrain sur une case.
data Tile = Road
          | Land
          | Water
  deriving (Eq, Show)
