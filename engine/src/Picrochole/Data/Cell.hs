{- |
   Module      : Picrochole.Data.Cell
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Vue d'une case du plateau de jeu.
-}

module Picrochole.Data.Cell
  ( CellContent
  , Cell
  , cellKey
  , cellTopography
  , cellCapacity
  , cellContent
  , getBlues
  , getReds
  , getFaction
  , getOpponents
  , getOpponents'
  , getStrongest
  , getCell
  , getDisk
  , getContested
  ) where

import Data.List ( maximumBy )
import qualified Data.Set as S

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Structs.XsMap
import Picrochole.Data.Units

-- | Contenu d'une cellule.
type CellContent = Either Faction (Bag Unit, Bag Unit)

-- | Une case du plateau de jeu.
data Cell = Cell { tile_ :: Tile
                 , cellKey_ :: CellKey
                 , cellContent_ :: CellContent
                 }
  deriving Show

-- | Renvoie l'identifiant de la case.
cellKey :: Cell -> CellKey
cellKey = cellKey_

-- | Renvoie la nature du terrain sur la case.
cellTopography :: Cell -> Topography
cellTopography c = topography (tile_ c)

-- | Renvoie la capacité maximale (par camp) de la case.
cellCapacity :: Cell -> Double
cellCapacity c = capacity (tile_ c)

-- | Renvoie le contenu de la cellule.
cellContent :: Cell -> CellContent
cellContent c = cellContent_ c

-- | Renvoie les unités bleues sur la case.
getBlues :: Cell -> Bag Unit
getBlues c = case cellContent c of
  Left _ -> emptyBag
  Right (xs, _) -> xs

-- | Renvoie les unités rouges sur la case.
getReds :: Cell -> Bag Unit
getReds c = case cellContent c of
  Left _ -> emptyBag
  Right (_, ys) -> ys

-- | Renvoie toutes les unités de la faction sur la case.
getFaction :: Faction -> Cell -> Bag Unit
getFaction Blue cell = getBlues cell
getFaction Red cell = getReds cell

-- | Renvoie toutes les unités adverses sur la case.
getOpponents :: Faction -> Cell -> Bag Unit
getOpponents Blue cell = getReds cell
getOpponents Red cell = getBlues cell

-- | Renvoie toutes les unités adverses sur la case.
getOpponents' :: Faction -> CellContent -> Bag Unit
getOpponents' _ (Left _) = emptyBag
getOpponents' Blue (Right (_, ys)) = ys
getOpponents' Red (Right (xs, _)) = xs

-- | Renvoie l'unité de la faction avec l'effectif le plus important sur la
-- case indiquée.
getStrongest :: Faction -> Cell -> Maybe Unit
getStrongest f cell = if null units
                      then Nothing
                      else Just (maximumBy comp units)
  where
    units = getFaction f cell
    comp x y = compare (strength x) (strength y)

-- | Renvoie une case du plateau de jeu.
getCell :: Atlas -> Units -> CellKey -> Cell
getCell atlas xs ck = Cell { tile_ = tile
                           , cellContent_ = content
                           , cellKey_ = ck
                           }
  where
    tile = getHex atlas ck
    content = case lookupLocation ck xs of
      Left f -> Left f
      Right units -> Right (blues, reds)
        where
          (blues, reds) = partition (\ x -> faction x == Blue) units

-- | Renvoie un disque de cases du plateau de jeu, dont le centre est la case
-- indiquée.
getDisk :: Atlas -> Units -> CellKey -> Int -> [Cell]
getDisk atlas xs ck radius = fmap (getCell atlas xs) keys
  where
    keys = getDiskKeys atlas ck radius

-- | Renvoie toutes les cases du plateau de jeu qui contiennent des unités
-- des deux camps.
getContested :: Atlas -> Units -> [Cell]
getContested atlas xs = fmap (getCell atlas xs) contested
  where
    blues = locations xs Blue
    reds = locations xs Red
    contested = S.toList (S.intersection blues reds)
