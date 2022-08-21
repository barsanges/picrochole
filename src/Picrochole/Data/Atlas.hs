{- |
   Module      : Picrochole.Data.Atlas
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Géographie du jeu.
-}

module Picrochole.Data.Atlas
  ( Atlas
  , CellParams(..)
  , module Picrochole.Data.Utils.HexGrid
  ) where

import Picrochole.Data.Base
import Picrochole.Data.Utils.HexGrid

-- | Paramètres immuables d'une case du plateau de jeu.
data CellParams = CP { tile_ :: Tile
                     , capacity_ :: Double
                     } -- FIXME : inscrire dans les types la distinction crossable / uncrossable
  deriving Show

-- | Géographie du jeu.
type Atlas = HexGrid CellParams
