{- |
   Module      : Picrochole.Misc.Position
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Position@ of an object on the game map.
-}

module Picrochole.Misc.Position (
  Position,
  Path,
  ) where

data Position = Position -- TODO
  deriving (Eq, Ord, Show)

type Path = [Position] -- FIXME : edges have different lengths