{- |
   Module      : Picrochole.Containers.Post
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Post@ is the collection of all messages exchanged between units in the game.
-}

module Picrochole.Containers.Post (
  module Picrochole.Misc.Messages,
  Post,
  troopsStoreSince
  ) where

import Picrochole.Actions.ActionID
import Picrochole.Misc.Messages
import Picrochole.Misc.Time

data Post = Post

-- | Get all messages received by a given store since a given date.
troopsStoreSince :: ActionID -> UTCTime -> Post -> [Troops_Store]
troopsStoreSince = undefined -- FIXME