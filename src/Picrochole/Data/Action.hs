{- |
   Module      : Picrochole.Data.Action
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Actions entreprises par les unités.
-}

module Picrochole.Data.Action
  ( Action(..)
  ) where

import Picrochole.Data.Keys

-- | Action entreprise par une unité.
data Action = Still
            | Moving Double [LocationKey]
  deriving (Show, Eq, Ord)
