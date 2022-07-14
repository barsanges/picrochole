{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , speed
  ) where

import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON(..), FromJSON(..), withScientific )
import Data.Aeson.Types ( Parser )
import Data.Scientific ( Scientific, toBoundedInteger )

-- | Numéro du tour en cours.
type TurnCount = Int

-- | Faction à laquelle appartient une unité.
data Faction = Blue
             | Red
  deriving (Eq, Generic, Show)

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

-- | Sérialisation.

instance ToJSON Faction
instance FromJSON Faction

instance ToJSON UnitKey where
  -- toJSON :: UnitKey -> Value
  toJSON (UK x) = toJSON x

instance FromJSON UnitKey where
  -- parseJSON :: Value -> Parser UnitKey
  parseJSON = withScientific "UnitKey" parseUnitKey

parseUnitKey :: Scientific -> Parser UnitKey
parseUnitKey v = case toBoundedInteger v of
  Just x -> return (UK x)
  Nothing -> fail ("unable to convert " ++ show v ++ " to an integer")

instance ToJSON UnitKind
instance FromJSON UnitKind

instance FromJSON Tile
instance ToJSON Tile
