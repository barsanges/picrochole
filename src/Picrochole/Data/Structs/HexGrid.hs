{-# LANGUAGE DeriveGeneric #-} -- FIXME : à supprimer à terme.
{- |
   Module      : Picrochole.Data.Structs.HexGrid
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Une grille hexagonale, dont les hexagones sont orientés pointe en haut.
-}

module Picrochole.Data.Structs.HexGrid
  ( HexGrid
  , GridSize(..)
  , CellKey
  , fromVector
  , toVector
  , gridSize
  , getHex
  , dist
  , touch
  , diskKeys
  ) where

import GHC.Generics ( Generic )
import Data.Vector ( Vector )
import qualified Data.Vector as V

-- | Dimensions de la grille.
data GridSize = GridSize { ncols :: Int
                         , nrows :: Int
                         }
  deriving (Eq, Show)

-- | Une grille hexagonale, dont les hexagones sont orientés pointe en haut.
data HexGrid a = HexGrid { cells :: Vector a
                         , gridSize_ :: GridSize
                         }
  deriving Show

-- | Identifiant unique d'une case.
newtype CellKey = CK Int
  deriving (Eq, Generic, Ord, Show)

-- | Construit une grille à partir d'un vecteur.
fromVector :: GridSize -> Vector a -> Maybe (HexGrid a)
fromVector gs xs = if (ncols gs) * (nrows gs) == V.length xs
                   then Just (HexGrid { cells = xs
                                      , gridSize_ = gs
                                      })
                   else Nothing

-- | Convertit une grille en un vecteur.
toVector :: HexGrid a -> Vector a
toVector = cells

-- | Renvoie les dimensions de la grille.
gridSize :: HexGrid a -> GridSize
gridSize = gridSize_

-- | Renvoie le contenu d'une cellule.
getHex :: HexGrid a -> CellKey -> a
getHex grid (CK idx) = (cells grid) V.! idx

-- | Convertit l'index en un couple de coordonnées pour une grille hexagonale.
toPair :: GridSize -> CellKey -> (Int, Int)
toPair gsize (CK a) = (a `mod` (ncols gsize), a `div` (ncols gsize))

-- | Convertit le couple de coordoonées en un index.
fromPair :: GridSize -> (Int, Int) -> CellKey
fromPair gsize (x, y) = CK (y * (ncols gsize) + x)

-- | Convertit l'index en un triplet de coordonnées pour une grille hexagonale.
-- Les hexagones sont orientés pointe en haut.
toHexCoord :: GridSize -> CellKey -> (Int, Int, Int)
toHexCoord gsize ckey = (q, r, -q-r)
  where
    (x, y) = toPair gsize ckey
    q = x - truncate (0.5 * fromIntegral (y - (y `mod` 2)) :: Double)
    r = y
-- FIXME : à terme, supprimer cette étape ?

-- | Convertit le triplet de coordonnées en une clef de lieu. Les hexagones sont
-- orientés pointe en haut.
fromHexCoord :: GridSize -> (Int, Int, Int) -> CellKey
fromHexCoord gsize (q, r, _) = fromPair gsize (x, y)
  where
    x = q + truncate (0.5 * fromIntegral (r - (r `mod` 2)) :: Double)
    y = r

-- | Calcule la distance en nombre de cases entre deux emplacements,
-- cf. [ici](https://www.redblobgames.com/grids/hexagons).
dist :: GridSize -> CellKey -> CellKey -> Int
dist gsize x y = truncate (0.5 * fromIntegral (diff) :: Double)
  where
    (xq, xr, xs) = toHexCoord gsize x
    (yq, yr, ys) = toHexCoord gsize y
    diff = abs (xq - yq) + abs (xr - yr) + abs (xs - ys)

-- | Indique si la case se situe dans la grille.
withinGrid :: GridSize -> CellKey -> Bool
withinGrid gsize ckey = 0 <= x && x < (ncols gsize) && 0 <= y && y < (nrows gsize)
  where
    (x, y) = toPair gsize ckey

-- | Indique si les cases `x` et `y` sont adjacentes.
touch :: GridSize -> CellKey -> CellKey -> Bool
touch gsize x y = (dist gsize x y) == 1

-- | Renvoie les identifiants d'un disque de cases de la grille, dont le centre
-- est la case indiquée.
diskKeys :: GridSize -> CellKey -> Int -> [CellKey]
diskKeys gsize ck radius = filter (withinGrid gsize) allKeys
  where
    (q, r, s) = toHexCoord gsize ck
    allKeys = [fromHexCoord gsize (q + dq, r + dr, s - (dq + dr))
               | dq <- [-radius..radius]
               , dr <- [(max (-radius) (-dq - radius))..(max radius (dq + radius))]
              ]
