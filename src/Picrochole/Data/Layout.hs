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
  , fromList
  , dist
  , lLookup
  , lLookupSeveral
  , bulkUpdate
  , neighbors
  ) where

import Data.Maybe ( catMaybes )
import Data.Vector ( (!?), (//) )
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
                    , lCivilianSupply :: Double
                    }
  deriving Show

-- | La carte, i.e. l'ensemble des cases de la carte.
data CLocations = CL { lVec :: V.Vector Location
                     , ncols :: Int
                     , nrows :: Int
                     }
  deriving Show

-- | Construit la carte à partir d'une liste de cases et d'un nombre de
-- colonnes.
fromList :: Int -> [Location] -> Maybe CLocations
fromList cols ls = if (cols * rows) == (V.length vec)
                    then Just (CL { lVec = vec
                                  , ncols = cols
                                  , nrows = rows
                                  })
                    else Nothing
  where
    vec = V.fromList ls
    rows = (V.length vec) `div` cols

-- | Convertit la clef en un indice entier permettant de requêter le vecteur des
-- cases.
toIntCoord :: CLocations -> LocationKey -> Int
toIntCoord cl (LK (x, y)) = x * (ncols cl) + y

-- | Convertit l'index en un triplet de coordonnées pour une grille hexagonale.
-- Les hexagones sont orientés pointe en haut.
toHexCoord :: LocationKey -> (Int, Int, Int)
toHexCoord (LK (x, y)) = (q, r, -q-r)
  where
    q = y - truncate (0.5 * fromIntegral (x - (x `mod` 2)) :: Double)
    r = x
-- FIXME : à terme, supprimer cette étape ?

-- | Convertit le triplet de coordonnées en une clef de lieu. Les hexagones sont
-- orientés pointe en haut.
fromHexCoord :: (Int, Int, Int) -> LocationKey
fromHexCoord (q, r, _) = LK (x, y)
  where
    y = q + truncate (0.5 * fromIntegral (r - (r `mod` 2)) :: Double)
    x = r

-- | Calcule la distance en nombre de cases entre deux emplacements,
-- cf. [ici](https://www.redblobgames.com/grids/hexagons).
dist :: LocationKey -> LocationKey -> Int
dist x y = truncate (0.5 * fromIntegral (diff) :: Double)
  where
    (xq, xr, xs) = toHexCoord x
    (yq, yr, ys) = toHexCoord y
    diff = abs (xq - yq) + abs (xr - yr) + abs (xs - ys)

-- | Renvoie la case correspondant à la clef fournie.
lLookup :: CLocations -> LocationKey -> Maybe Location
lLookup cl lk = (lVec cl) !? (toIntCoord cl lk)

-- | Renvoie les cases correspondant aux clefs fournies.
lLookupSeveral :: CLocations -> [LocationKey] -> [Location]
lLookupSeveral cl lks = catMaybes (fmap (lLookup cl) lks)

-- | Met à jour la carte pour les emplacements donnés.
bulkUpdate :: CLocations -> [(LocationKey, Location)] -> CLocations
bulkUpdate cl xs = cl { lVec = v' }
  where
    v = lVec cl
    v' = v // (fmap (\ (k, l) -> (toIntCoord cl k, l))) xs

-- | Renvoie l'ensemble des cases situées dans le rayon `radius` autour d'une
-- case initiale. La case initiale est incluse dans le résultat.
neighbors :: CLocations -> LocationKey -> Int -> [LocationKey]
neighbors cl lk radius = if radius >= 0
                         then filter filtering (fmap fromHexCoord hexs)
                         else error "negative radius in function 'neighbors'"
  where
    (q, r, s) = toHexCoord lk
    hexs = [ (q + dq, r + dr, s + ds)
           | dq <- [-radius..radius]
           , dr <- [(max (-radius - dq) (-radius))..(min (radius - dq) radius)]
           , let ds = -(dq + dr) ]
    len = V.length (lVec cl)
    filtering :: LocationKey -> Bool
    filtering x = (0 <= i) && (i < len)
      where
        i = toIntCoord cl x
