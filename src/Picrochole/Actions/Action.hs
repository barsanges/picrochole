{- |
   Module      : Picrochole.Actions.Action
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Action@ is a wrapper for functions that update world (e.g. the game state).
-}

module Picrochole.Actions.Action (
  Action(..)
  ) where

import Data.Sequence ( Seq )

import Picrochole.Containers.World

data Action = Action { apply :: World -> (Seq Action, World) }