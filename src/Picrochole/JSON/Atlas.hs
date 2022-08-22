{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Atlas
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON de la géographie du jeu.
-}

module Picrochole.JSON.Atlas
  ( Atlas(..)
  , Tile(..)
  , topography
  , capacity
  ) where

import Data.Aeson
import qualified Data.Text as T
import Data.Vector ( Vector )

import Picrochole.JSON.Utils

-- | Paramètres d'une case du plateau de jeu.
data Tile = Crossable T.Text Double
          | Uncrossable T.Text
  deriving Show

-- | Renvoie la nature du terrain associé à la case.
topography :: Tile -> T.Text
topography (Crossable t _) = normalize t
topography (Uncrossable t) = normalize t

-- | Renvoie la capacité de la case.
capacity :: Tile -> Double
capacity (Crossable _ c) = c
capacity (Uncrossable _) = 0

instance FromJSON Tile where
  parseJSON = withObject "Tile" go
    where
      go v = do
        t <- v .: "topography"
        mc <- v .:? "capacity"
        case mc of
          Just c -> return (Crossable t c)
          Nothing -> return (Uncrossable t)

-- | Géographie du jeu.
data Atlas = Atlas { ncols :: Int
                   , nrows :: Int
                   , content :: Vector Tile
                   }
  deriving Show

instance FromJSON Atlas where
  parseJSON = withObject "Atlas" go
    where
      go v = do
        nc <- v .: "ncols"
        nr <- v .: "nrows"
        c <- v .: "content"
        return Atlas { ncols = nc
                     , nrows = nr
                     , content = c
                     }
