{- |
   Module      : Picrochole.Data.Structs.HexGrid
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Une grille hexagonale, dont les hexagones sont orientés pointe en haut.
-}

module Picrochole.Data.Structs.HexGrid
  ( HexGrid
  , GridSize(..)
  , CellKey(..)
  , fromVector
  , toVector
  , gridSize
  , isInsideGrid
  , getHex
  , dist
  , touch
  , diskKeys
  ) where

import Data.Maybe ( catMaybes )
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
  deriving (Eq, Show)

-- | Identifiant unique d'une case.
newtype CellKey = CK Int
  deriving (Eq, Ord, Show)

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

-- | Indique si l'index correspond bien à une case de la grille.
isInsideGrid :: HexGrid a -> CellKey -> Bool
isInsideGrid grid (CK idx) = (0 <= idx) && (idx < V.length (cells grid))

-- | Renvoie le contenu d'une cellule.
getHex :: HexGrid a -> CellKey -> a
getHex grid (CK idx) = (cells grid) V.! idx

-- | Convertit l'index en un triplet de coordonnées pour une grille hexagonale.
-- Les hexagones sont orientés pointe en haut.
toHexCoord :: GridSize -> CellKey -> Maybe (Int, Int, Int)
toHexCoord gs (CK idx) = if (0 <= idx) && (idx < (ncols gs) * (nrows gs))
                         then Just (q, r, -q-r)
                         else Nothing
  where
    r = idx `div` (ncols gs)
    q = (idx `mod` (ncols gs)) - truncate (0.5 * fromIntegral (r - (r `mod` 2)) :: Double)

-- | Convertit le triplet de coordonnées en une clef de lieu. Les hexagones sont
-- orientés pointe en haut.
fromHexCoord :: GridSize -> (Int, Int, Int) -> Maybe CellKey
fromHexCoord gs (q, r, _) =
  if (0 <= x) && (x < ncols gs) && (0 <= r) && (r < nrows gs)
  then Just (CK (r * (ncols gs) + x))
  else Nothing
  where
    x = q + truncate (0.5 * fromIntegral (r - (r `mod` 2)) :: Double)

-- | Calcule la distance en nombre de cases entre deux emplacements,
-- cf. [ici](https://www.redblobgames.com/grids/hexagons).
dist :: GridSize -> CellKey -> CellKey -> Maybe Int
dist gsize x y = do
  (xq, xr, xs) <- toHexCoord gsize x
  (yq, yr, ys) <- toHexCoord gsize y
  let diff = abs (xq - yq) + abs (xr - yr) + abs (xs - ys)
  return (truncate (0.5 * fromIntegral (diff) :: Double))

-- | Indique si les cases `x` et `y` sont adjacentes.
touch :: GridSize -> CellKey -> CellKey -> Bool
touch gsize x y = (dist gsize x y) == (Just 1)

-- | Renvoie les identifiants d'un disque de cases de la grille, dont le centre
-- est la case indiquée.
diskKeys :: GridSize -> CellKey -> Int -> [CellKey]
diskKeys gsize ck radius = case toHexCoord gsize ck of
  Nothing -> []
  Just (q, r, s) -> catMaybes allKeys
    where
      allKeys = [fromHexCoord gsize (q + dq, r + dr, s - (dq + dr))
                | dq <- [-radius..radius]
                , dr <- [(max (-radius) (-dq - radius))..(min radius (-dq + radius))]
                ]
