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

-- | Rapport sur une unité.
data Unit = Unit { unitKey :: Text
                 , faction :: Text
                 , kind :: Text
                 , strength :: Double
                 , progress :: Maybe Double
                 }
  deriving Show

-- | Rapport sur une cellule.
data CellContent = Units (Vector Unit)
                 | Marker Text
  deriving Show

-- | Rapport d'une unité à son état-major.
data Report = Report { from :: Text
                     , to :: Text
                     , sent :: Int
                     , received :: Maybe Int
                     , content :: Map Text CellContent
                     }
  deriving Show

instance FromJSON Unit where
  parseJSON = withObject "Unit" go
    where
      go v = do
        uk <- v .: "unit-key"
        f <- v .: "faction"
        k <- v .: "kind"
        s <- v .: "strength"
        p <- v .: "progress"
        return Unit { unitKey = uk
                    , faction = f
                    , kind = k
                    , strength = s
                    , progress = p
                    }

instance ToJSON Unit where
  toJSON x = object [ "key" .= unitKey x
                    , "faction" .= faction x
                    , "kind" .= kind x
                    , "strength" .= strength x
                    , "progress" .= progress x
                    ]

instance FromJSON CellContent where
  parseJSON (String t) = return (Marker t)
  parseJSON (Array xs) = Units <$> (parseJSON (Array xs))
  parseJSON _ = fail "for a given cell, the report shoud either contain a faction ID or an array of units"

instance ToJSON CellContent where
  toJSON (Marker t) = String t
  toJSON (Units xs) = toJSON xs

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
