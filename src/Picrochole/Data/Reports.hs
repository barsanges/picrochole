{- |
   Module      : Picrochole.Data.Reports
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les rapports que les unités s'échangent.
-}

module Picrochole.Data.Reports
  ( Report
  , module Picrochole.Data.Structs.Register
  ) where

import Data.Map ( Map )

import Picrochole.Data.Atlas ( CellKey )
import Picrochole.Data.Board ( CellContent )
import Picrochole.Data.Structs.Register

-- | Rapport d'une unité à son état-major.
type Report = Map CellKey CellContent
