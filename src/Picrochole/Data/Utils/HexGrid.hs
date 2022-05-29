{- |
   Module      : Picrochole.Data.Utils.HexGrid
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Une grille hexagonale, dont les hexagones sont orientés pointe en haut.
-}

module Picrochole.Data.Utils.HexGrid
  ( HexGrid
  , CellKey
  , getHex
  , dist
  , touch
  , diskKeys
  ) where

import Data.Vector ( Vector )
import qualified Data.Vector as V

-- | Une grille hexagonale, dont les hexagones sont orientés pointe en haut.
data HexGrid a = HexGrid { cells :: Vector a
                         , ncols :: Int
                         , nrows :: Int
                         }
  deriving Show

-- | Identifiant unique d'une case.
data CellKey = CK Int Int
  deriving (Eq, Ord, Show)

-- | Renvoie le contenu d'une cellule.
getHex :: HexGrid a -> CellKey -> a
getHex grid ckey = case toIntCoord grid ckey of
                     Just idx -> ((cells grid) V.! idx)
                     Nothing -> error ("unknown cell " ++ show ckey)

-- | Convertit la clef en un indice entier permettant de requêter le vecteur des
-- cases.
toIntCoord :: HexGrid a -> CellKey -> Maybe Int
toIntCoord grid (CK x y) = if withinGrid grid (CK x y)
                           then Just (y * (ncols grid) + x)
                           else Nothing

-- | Convertit l'index en un triplet de coordonnées pour une grille hexagonale.
-- Les hexagones sont orientés pointe en haut.
toHexCoord :: CellKey -> (Int, Int, Int)
toHexCoord (CK x y) = (q, r, -q-r)
  where
    q = x - truncate (0.5 * fromIntegral (y - (y `mod` 2)) :: Double)
    r = y
-- FIXME : à terme, supprimer cette étape ?

-- | Convertit le triplet de coordonnées en une clef de lieu. Les hexagones sont
-- orientés pointe en haut.
fromHexCoord :: (Int, Int, Int) -> CellKey
fromHexCoord (q, r, _) = CK x y
  where
    x = q + truncate (0.5 * fromIntegral (r - (r `mod` 2)) :: Double)
    y = r

-- | Calcule la distance en nombre de cases entre deux emplacements,
-- cf. [ici](https://www.redblobgames.com/grids/hexagons).
dist :: CellKey -> CellKey -> Int
dist x y = truncate (0.5 * fromIntegral (diff) :: Double)
  where
    (xq, xr, xs) = toHexCoord x
    (yq, yr, ys) = toHexCoord y
    diff = abs (xq - yq) + abs (xr - yr) + abs (xs - ys)

-- | Indique si la case se situe dans la grille.
withinGrid :: HexGrid a -> CellKey -> Bool
withinGrid g (CK x y) = 0 <= x && x < (ncols g) && 0 <= y && y < (nrows g)

-- | Indique si les cases `x` et `y` sont adjacentes.
touch :: CellKey -> CellKey -> Bool
touch x y = (dist x y) == 1

-- | Renvoie les identifiants d'un disque de cases de la grille, dont le centre
-- est la case indiquée.
diskKeys :: HexGrid a -> CellKey -> Int -> [CellKey]
diskKeys grid ck radius = filter (withinGrid grid) allKeys
  where
    (q, r, s) = toHexCoord ck
    allKeys = [fromHexCoord (q + dq, r + dr, s - (dq + dr))
               | dq <- [-radius..radius]
               , dr <- [(max (-radius) (-dq - radius))..(max radius (dq + radius))]
              ]
