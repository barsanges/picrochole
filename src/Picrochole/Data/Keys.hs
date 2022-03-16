{- |
   Module      : Picrochole.Data.Keys
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Identifiants pour les différents éléments du jeu.
-}

module Picrochole.Data.Keys
  ( LocationKey(..)
  , UnitKey(..)
  , FactionKey(..)
  , StoreKey(..)
  , HasLocation(..)
  ) where

-- | Clef associée à un emplacement sur la carte.
newtype LocationKey = LK Int
  deriving (Show, Eq, Ord)

-- | Clef associée à une unité.
newtype UnitKey = UK Int
  deriving (Show, Eq, Ord)

-- | Clef associée à une faction.
newtype FactionKey = FK Int
  deriving (Show, Eq, Ord)

-- | Clef associée à un dépôt.
newtype StoreKey = SK Int
  deriving (Show, Eq, Ord)

-- | Classe pour les types contenant une clef de lieu.
class HasLocation a where
  getLocation :: a -> LocationKey
