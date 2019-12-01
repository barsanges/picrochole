{- |
   Module      : Picrochole.Actions.Action
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

An @ActionID@ is a key making the link between an @Action@ and the associated
object in the world.
-}

module Picrochole.Actions.ActionID (
  ActionID(..)
  ) where

newtype ActionID = ActionID String
  deriving (Eq, Ord, Show)