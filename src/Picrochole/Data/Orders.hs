{- |
   Module      : Picrochole.Data.Orders
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les orders que les unités s'échangent.
-}

module Picrochole.Data.Orders
  ( Order
  , module Picrochole.Data.Structs.Register
  ) where

import Picrochole.Data.Atlas ( CellKey )
import Picrochole.Data.Structs.Register

-- | Ordre de l'état-major à un subordonné.
type Order = CellKey
