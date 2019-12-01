{- |
   Module      : Picrochole.Containers.World
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@World@ contains the game state.
-}

module Picrochole.Containers.World (
  module Picrochole.Containers.Graph,
  World(..)
  ) where

import Picrochole.Containers.Graph
import Picrochole.Containers.Post
import Picrochole.Misc.Time

data World = World
  {
    currentTime :: UTCTime,
    graph :: Graph,
    post :: Post
  }