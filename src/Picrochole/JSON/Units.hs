{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Units
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON des unités des deux camps.
-}

module Picrochole.JSON.Units
  ( Units
  , Unit(..)
  , CellContent(..)
  ) where

import Data.Aeson
import Data.Map ( Map )
import Data.Text ( Text )
import Data.Vector ( Vector )

-- | Une unité du plateau de jeu.
data Unit = Unit { unitKey :: Text
                 , faction :: Text
                 , kind :: Text
                 , strength :: Double
                 , progress :: Maybe Double
                 }
  deriving Show

-- | Contenu d'une cellule.
data CellContent = Units (Vector Unit)
                 | Marker Text
  deriving Show

-- | Les unités des deux camps.
type Units = Map Text CellContent

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
  parseJSON _ = fail "a given cell should either contain a faction ID or an array of units"

instance ToJSON CellContent where
  toJSON (Marker t) = String t
  toJSON (Units xs) = toJSON xs
