{- |
   Module      : Picrochole.Data.Utils.HexGrid
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Une grille hexagonale, dont les hexagones sont orientés pointe en haut.
-}

module Picrochole.Data.Utils.HexGrid
  ( HexGrid
  , GridSize(..)
  , CellKey
  , gridSize
  , getHex
  , dist
  , touch
  , diskKeys
  ) where

import Data.Aeson
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
data CellKey = CK Int Int
  deriving (Eq, Ord, Show)

-- | Renvoie les dimensions de la grille.
gridSize :: HexGrid a -> GridSize
gridSize = gridSize_

-- | Renvoie le contenu d'une cellule.
getHex :: HexGrid a -> CellKey -> a
getHex grid ckey = case toIntCoord (gridSize grid) ckey of
                     Just idx -> ((cells grid) V.! idx)
                     Nothing -> error ("unknown cell " ++ show ckey)

-- | Convertit la clef en un indice entier permettant de requêter le vecteur des
-- cases.
toIntCoord :: GridSize -> CellKey -> Maybe Int
toIntCoord gsize (CK x y) = if withinGrid gsize (CK x y)
                           then Just (y * (ncols gsize) + x)
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
withinGrid :: GridSize -> CellKey -> Bool
withinGrid gsize (CK x y) = 0 <= x && x < (ncols gsize) && 0 <= y && y < (nrows gsize)

-- | Indique si les cases `x` et `y` sont adjacentes.
touch :: CellKey -> CellKey -> Bool
touch x y = (dist x y) == 1

-- | Renvoie les identifiants d'un disque de cases de la grille, dont le centre
-- est la case indiquée.
diskKeys :: GridSize -> CellKey -> Int -> [CellKey]
diskKeys gsize ck radius = filter (withinGrid gsize) allKeys
  where
    (q, r, s) = toHexCoord ck
    allKeys = [fromHexCoord (q + dq, r + dr, s - (dq + dr))
               | dq <- [-radius..radius]
               , dr <- [(max (-radius) (-dq - radius))..(max radius (dq + radius))]
              ]

-- | Sérialisation.

instance ToJSON CellKey where
  -- toJSON :: CellKey -> Value
  toJSON (CK x y) = Array (V.fromList [toJSON x, toJSON y])

instance FromJSON CellKey where
  -- parseJSON :: Value -> Parser CellKey
  parseJSON = withArray "CellKey" go
    where
      go xs =
        if V.length xs == 2
        then do
          x <- parseJSON (xs V.! 0)
          y <- parseJSON (xs V.! 1)
          return (CK x y)
        else fail ("expected an array of length 2, got " ++ show (V.length xs) ++ " instead")
