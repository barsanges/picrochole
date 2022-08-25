{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Reports
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON des rapports que les unités s'échangent.
-}

module Picrochole.JSON.Reports
  ( Reports
  , Report(..)
  , Unit(..)
  , CellContent(..)
  ) where

import Data.Aeson
import Data.Map ( Map )
import Data.Text ( Text )
import Data.Vector ( Vector )

import Picrochole.JSON.Units ( Unit(..), CellContent(..) )

-- | Rapport d'une unité à son état-major.
data Report = Report { from :: Text
                     , to :: Text
                     , sent :: Int
                     , received :: Maybe Int
                     , content :: Map Text CellContent
                     }
  deriving Show

instance FromJSON Report where
  parseJSON = withObject "Report" go
    where
      go v = do
        f <- v .: "from"
        t <- v .: "to"
        s <- v .: "sent"
        r <- v .: "received"
        c <- v .: "content"
        return Report { from = f
                      , to = t
                      , sent = s
                      , received = r
                      , content = c
                      }

instance ToJSON Report where
  toJSON x = object [ "from" .= from x
                    , "to" .= to x
                    , "sent" .= sent x
                    , "received" .= received x
                    , "content" .= content x
                    ]

-- | Rapports que les unités s'échangent.
type Reports = Vector Report
