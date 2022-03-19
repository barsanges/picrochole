{- |
   Module      : Picrochole.Data.Layout
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour la disposition du terrain.
-}

module Picrochole.Data.Layout
  ( Ground(..)
  , Location(..)
  , CLocations
  , dist
  ) where

import qualified Data.Vector as V
import Picrochole.Data.Keys

-- | Type de terrain associé à une case de la carte.
data Ground = Plain
            | Forest
            | Road
            | City
            | River
            | Bridge
  deriving (Show, Eq)

-- | Une case de la carte.
data Location = Loc { ground :: Ground
                    , height :: Int
                    }
  deriving Show

-- | La carte, i.e. l'ensemble des cases de la carte.
data CLocations = CL { lVec :: V.Vector Location
                     , ncols :: Int
                     , nrows :: Int
                     }
  deriving Show

-- | Convertit l'index en un triplet de coordonnées pour une grille hexagonale.
-- Les hexagones sont orientés pointe en haut.
toHexCoord :: LocationKey -> (Int, Int, Int)
toHexCoord (LK (x, y)) = (q, r, -q-r)
  where
    q = y - truncate (0.5 * fromIntegral (x - (x `mod` 2)) :: Double)
    r = x
-- FIXME : à terme, supprimer cette étape ?

-- | Calcule la distance en nombre de cases entre deux emplacements,
-- cf. [ici](https://www.redblobgames.com/grids/hexagons).
dist :: LocationKey -> LocationKey -> Int
dist x y = truncate (0.5 * fromIntegral (diff) :: Double)
  where
    (xq, xr, xs) = toHexCoord x
    (yq, yr, ys) = toHexCoord y
    diff = abs (xq - yq) + abs (xr - yr) + abs (xs - ys)
