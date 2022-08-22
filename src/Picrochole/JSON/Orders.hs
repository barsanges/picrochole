{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Orders
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON des ordres que les unités s'échangent.
-}

module Picrochole.JSON.Orders
  ( Order(..)
  , Orders
  ) where

import Data.Aeson
import Data.Text ( Text )
import Data.Vector ( Vector )

-- | Ordre de l'état-major à un subordonné.
data Order = Order { from :: Text
                   , to :: Text
                   , sent :: Int
                   , received :: Int
                   , content :: Int
                   }
  deriving Show

instance FromJSON Order where
  parseJSON = withObject "Order" go
    where
      go v = do
        f <- v .: "from"
        t <- v .: "to"
        s <- v .: "sent"
        r <- v .: "received"
        c <- v .: "content"
        return Order { from = f
                     , to = t
                     , sent = s
                     , received = r
                     , content = c
                     }

instance ToJSON Order where
  toJSON x = object [ "from" .= from x
                    , "to" .= to x
                    , "sent" .= sent x
                    , "received" .= received x
                    , "content" .= content x
                    ]

-- | Ordres que les unités s'échangent.
type Orders = Vector Order
