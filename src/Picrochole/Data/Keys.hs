{- |
   Module      : Picrochole.Data.Keys
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Identifiants pour les différents éléments du jeu.
-}

module Picrochole.Data.Keys
  ( LocationKey(..)
  , UnitKey(..)
  ) where

-- | Clef associée à un emplacement sur la carte.
newtype LocationKey = LK Int
  deriving (Show, Eq)

-- | Clef associée à une unité.
newtype UnitKey = UK Int
  deriving (Show, Eq, Ord)
