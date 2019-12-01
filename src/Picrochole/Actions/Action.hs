{- |
   Module      : Picrochole.Actions.Action
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Action@ is a wrapper for functions that update world (e.g. the game state).
-}

module Picrochole.Actions.Action (
  Action(..),
  ActionID(..)
  ) where

import Data.Sequence ( Seq )

import Picrochole.Containers.World

-- | @Action@ is a wrapper for functions that update world (e.g. the game state).
data Action = Action { apply :: World -> (Seq Action, World) }

-- | An @ActionID@ is a key making the link between an @Action@ and the
-- associated object in the world.
newtype ActionID = ActionID String
  deriving (Eq, Ord, Show)