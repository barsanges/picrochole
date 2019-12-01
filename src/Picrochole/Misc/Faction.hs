{- |
   Module      : Picrochole.Misc.Faction
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

Belligerents are divided into different factions, identified by the @Faction@
type.
-}

module Picrochole.Misc.Faction (
  Faction
  ) where

newtype Faction = Faction String
  deriving (Eq, Show)