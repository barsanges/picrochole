{- |
   Module      : Picrochole.Data.Board
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Le plateau de jeu et les unités des deux camps.
-}

module Picrochole.Data.Board
  ( module Picrochole.Data.Units
  , CellKey
  , CellContent
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
  , Board
  , mkBoard
  , touch
  , allUnits
  , getUnit
  , getDist
  , decrStrength
  , getPosition
  , setPosition
  , getLocation
  , getLocations
  , getCell
  , getDisk
  , getDiskKeys
  , getContested
  , removeFaction
  , capacityLeft
  , getMarker
  , setMarker
  , isContested
  , hasUnit
  ) where

import Data.List ( maximumBy, partition )
import Data.Set ( Set )
import qualified Data.Set as S

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Structs.XsMap
import Picrochole.Data.Units

-- | Contenu d'une cellule.
type CellContent = Either Faction ([Unit], [Unit])

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
getBlues :: Cell -> [Unit]
getBlues c = case cellContent c of
  Left _ -> []
  Right (xs, _) -> xs

-- | Renvoie les unités rouges sur la case.
getReds :: Cell -> [Unit]
getReds c = case cellContent c of
  Left _ -> []
  Right (_, ys) -> ys

-- | Renvoie toutes les unités de la faction sur la case.
getFaction :: Faction -> Cell -> [Unit]
getFaction Blue cell = getBlues cell
getFaction Red cell = getReds cell

-- | Renvoie toutes les unités adverses sur la case.
getOpponents :: Faction -> Cell -> [Unit]
getOpponents Blue cell = getReds cell
getOpponents Red cell = getBlues cell

-- | Renvoie toutes les unités adverses sur la case.
getOpponents' :: Faction -> CellContent -> [Unit]
getOpponents' _ (Left _) = []
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

-- | Le plateau de jeu avec l'ensemble des unités des deux camps.
data Board = Board { bXsMap :: XsMap CellKey UnitKey Faction Unit }
  deriving Show

-- | Renvoie une nouvelle instance de `Board`.
mkBoard :: XsMap CellKey UnitKey Faction Unit -> Board
mkBoard xs = Board { bXsMap = xs }

-- | Renvoie les unités du plateau de jeu.
allUnits :: Board -> XsMap CellKey UnitKey Faction Unit
allUnits b = bXsMap b

-- | Renvoie une unité du plateau de jeu.
getUnit :: Board -> UnitKey -> Maybe Unit
getUnit board ukey = lookupKey ukey (bXsMap board)

-- | Renvoie la distance à vol d'oiseau entre deux unités.
getDist :: Atlas -> Board -> UnitKey -> UnitKey -> Maybe Int
getDist atlas board x y = do
  x' <- lookupKey x (bXsMap board)
  y' <- lookupKey y (bXsMap board)
  return (dist (gridSize atlas) (location x') (location y'))

-- | Supprime une unité du plateau de jeu.
removeUnit :: UnitKey -> Board -> Board
removeUnit uk board = board { bXsMap = xs' }
  where
    xs' = deleteKey uk (bXsMap board)

-- | Diminue la force d'une unité sur le plateau de jeu.
-- FIXME : à supprimer à terme.
decrStrength :: UnitKey -> Double -> Board -> Board
decrStrength uk ds board = board { bXsMap = xs' }
  where
    xs' = decrStrength' uk ds (bXsMap board)

-- | Renvoie la position d'une unité sur le plateau de jeu.
-- FIXME : à déplacer dans Units.hs à terme.
getPosition :: Board -> UnitKey -> Maybe Position
getPosition board ukey = fmap position (lookupKey ukey (bXsMap board))

-- | Change la position d'une unité sur le plateau de jeu.
-- FIXME : à supprimer à terme.
setPosition :: UnitKey -> Position -> Board -> Board
setPosition ukey pos board = board { bXsMap = xs' }
  where
    xs' = setPosition' ukey pos (bXsMap board)

-- | Renvoie l'emplacement d'une unité du plateau de jeu.
getLocation :: Board -> UnitKey -> Maybe CellKey
getLocation board ukey = fmap location (lookupKey ukey (bXsMap board))

-- | Renvoie l'emplacement de toutes les unités d'un camp.
getLocations :: Board -> Faction -> Set CellKey
getLocations board f = foldr go S.empty (bXsMap board)
  where
    go :: Unit -> Set CellKey -> Set CellKey
    go u s = if faction u == f
             then S.insert (location u) s
             else s

-- | Renvoie une case du plateau de jeu.
getCell :: Atlas -> Board -> CellKey -> Cell
getCell atlas board ck = Cell { tile_ = tile
                              , cellContent_ = content
                              , cellKey_ = ck
                              }
  where
    tile = getHex atlas ck
    content = case lookupLocation ck (bXsMap board) of
      Left f -> Left f
      Right units -> Right (blues, reds)
        where
          (blues, reds) = partition (\ x -> faction x == Blue) units

-- | Renvoie les identifiants d'un disque de cases du plateau de jeu, dont le
-- centre est la case indiquée.
getDiskKeys :: Atlas -> CellKey -> Int -> [CellKey]
getDiskKeys atlas ck radius = diskKeys (gridSize atlas) ck radius

-- | Renvoie un disque de cases du plateau de jeu, dont le centre est la case
-- indiquée.
getDisk :: Atlas -> Board -> CellKey -> Int -> [Cell]
getDisk atlas board ck radius = fmap (getCell atlas board) keys
  where
    keys = getDiskKeys atlas ck radius

-- | Renvoie toutes les cases du plateau de jeu qui contiennent des unités
-- des deux camps.
getContested :: Atlas -> Board -> [Cell]
getContested atlas board = fmap (getCell atlas board) contested
  where
    blues = getLocations board Blue
    reds = getLocations board Red
    contested = S.toList (S.intersection blues reds)

-- | Supprime toutes les unités d'un camp sur la case donnée.
removeFaction :: Faction -> CellKey -> Board -> Board
removeFaction f ck board = foldr go board bunits
  where
    bunits = lookupLocationContent ck (bXsMap board)

    go :: Unit -> Board -> Board
    go u b = if faction u == f
             then removeUnit (unitKey u) b
             else b

-- | Renvoie la capacité d'accueil restante de la case donnée.
capacityLeft :: Atlas -> Board -> Faction -> CellKey -> Double
capacityLeft atlas board f ck = foldr go maxCapacity bunits
  where
    maxCapacity = capacity (getHex atlas ck)
    bunits = lookupLocationContent ck (bXsMap board)

    go :: Unit -> Double -> Double
    go u c = if faction u == f
             then c - (strength u)
             else c

-- | Renvoie le marqueur sur la case donnée.
getMarker :: Board -> CellKey -> Maybe Faction
getMarker board ck = lookupLocationToken ck (bXsMap board)

-- | Met à jour le marqueur sur la case donnée.
setMarker :: Faction -> CellKey -> Board -> Board
setMarker f ck board = board { bXsMap = b' }
  where
    b' = insertToken ck f (bXsMap board)

-- | Indique si la case donnée contient une unité du camp indiqué.
hasUnit :: Board -> Faction -> CellKey -> Bool
hasUnit board f x = any go (lookupLocationContent x (bXsMap board))
  where
    go :: Unit -> Bool
    go u = (faction u) == f

-- | Indique si la case donnée contient des unités des deux camps.
isContested :: Board -> CellKey -> Bool
isContested board x = (hasUnit board Blue x) && (hasUnit board Red x)
