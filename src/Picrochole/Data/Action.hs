{- |
   Module      : Picrochole.Data.Action
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Actions entreprises par les unités.
-}

module Picrochole.Data.Action
  ( Action(..)
  , CActions
  ) where

import qualified Data.Map as M -- FIXME : IntMap ?
import Picrochole.Data.Keys

-- | Action entreprise par une unité.
data Action = Still
  deriving (Show, Eq)

-- | L'action associée à chaque unité.
newtype CActions = CA (M.Map UnitKey Action)
