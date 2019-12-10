{- |
   Module      : Picrochole.Misc.Messages
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

Messages exchanged between entities during the game.

Note : for a message of type "A_B", "A" stands for the type of the sender and
"B" for the type of the recipient.
-}

{- HLINT ignore "Use camelCase" -}

module Picrochole.Misc.Messages (
  Message(..),
  Troops_Store(..),
  sender,
  senderFaction,
  senderPosition,
  recipient
  ) where

import Picrochole.Actions.ActionID
import Picrochole.Misc.Faction
import Picrochole.Misc.Position

data Meta = Meta { _sender :: ActionID,
                   _senderFaction :: Faction,
                   _senderPosition :: Position,
                   _recipient :: ActionID
                 }

data Troops_Store = Troops_Store { metaTroops_Store :: Meta,
                                   qtyNeeded :: Double
                                 }

class Message a where
  meta :: a -> Meta

instance Message Troops_Store where
  meta = metaTroops_Store

sender :: (Message a) => a -> ActionID
sender = _sender . meta

senderFaction :: (Message a) => a -> Faction
senderFaction = _senderFaction . meta

senderPosition :: (Message a) => a -> Position
senderPosition = _senderPosition . meta

recipient :: (Message a) => a -> ActionID
recipient = _recipient . meta
