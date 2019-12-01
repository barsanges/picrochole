{- |
   Module      : Picrochole.Engine
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

Functions sequencing modifications (@Action@) of the game state (@World@).
-}

module Picrochole.Engine (
  inch
  ) where

import Data.Sequence ( Seq(..), (><) )

import Picrochole.Actions.Action
import Picrochole.Containers.World

inch :: Seq Action -> World -> (Seq Action, World)
inch Empty w = (Empty, w)
inch (x :<| xs) w = (xs >< new, w')
  where
    (new, w') = apply x w