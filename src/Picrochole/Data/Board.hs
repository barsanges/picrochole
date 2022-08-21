{- |
   Module      : Picrochole.Data.Board
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Le plateau de jeu et les unités des deux camps.
-}

module Picrochole.Data.Board
  ( Unit
  , Position(..)
  , unitKey
  , faction
  , kind
  , strength
  , location
  , progress
  , mkUnit
  , CellKey
  , CellContent
  , Cell
  , cellKey
  , tile
  , capacity
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
  , initiative
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
  , tile'
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
import Picrochole.Data.Utils.XsMap

-- | Paramètres immuables d'une unité.
data UnitParams = UP { unitKey_ :: UnitKey
                     , faction_ :: Faction
                     , kind_ :: UnitKind
                     }
  deriving Show

-- | Une unité du plateau de jeu.
data Unit = Unit { unitParams :: UnitParams
                 , strength_ :: Double
                 , position_ :: Position
                 }
  deriving Show

-- | Position d'une unité sur le plateau de jeu.
data Position = Position { currentCell :: CellKey
                         , currentProgress :: Maybe Double
                         }
  deriving Show

-- | Renvoie l'identifiant de l'unité.
unitKey :: Unit -> UnitKey
unitKey u = unitKey_ (unitParams u)

-- | Renvoie la faction de l'unité.
faction :: Unit -> Faction
faction u = faction_ (unitParams u)

-- | Renvoie l'arme de l'unité.
kind :: Unit -> UnitKind
kind u = kind_ (unitParams u)

-- | Renvoie la force de l'unité.
strength :: Unit -> Double
strength = strength_

-- | Renvoie l'identifiant de la cellule sur laquelle se trouve l'unité.
location :: Unit -> CellKey
location = currentCell . position_

-- | Renvoie l'état d'avancement de l'unité sur sa case.
progress :: Unit -> Maybe Double
progress = currentProgress . position_

-- | Renvoie une unité.
mkUnit :: UnitKey
       -> Faction
       -> UnitKind
       -> Double
       -> CellKey
       -> Maybe Double
       -> Unit
mkUnit ky f ki s l p = Unit { unitParams = UP { unitKey_ = ky
                                              , faction_ = f
                                              , kind_ = ki
                                              }
                            , strength_ = s
                            , position_ = Position { currentCell = l
                                                   , currentProgress = p
                                                   }
                            }

-- | Contenu d'une cellule.
type CellContent = Either Faction ([Unit], [Unit])

-- | Une case du plateau de jeu.
data Cell = Cell { cellParams :: CellParams
                 , cellKey_ :: CellKey
                 , cellContent_ :: CellContent
                 }
  deriving Show

-- | Renvoie l'identifiant de la case.
cellKey :: Cell -> CellKey
cellKey = cellKey_

-- | Renvoie la nature du terrain sur la case.
tile :: Cell -> Tile
tile c = tile_ (cellParams c)

-- | Renvoie la capacité maximale (par camp) de la case.
capacity :: Cell -> Double
capacity c = capacity_ (cellParams c)

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
data Board = Board { bXsMap :: XsMap CellKey UnitKey Faction Unit
                   , bInitiative :: [UnitKey]
                   }
  deriving Show

-- | Renvoie une nouvelle instance de `Board`.
mkBoard :: XsMap CellKey UnitKey Faction Unit
        -> [UnitKey]
        -> Board
mkBoard xs is = Board { bXsMap = xs
                      , bInitiative = is
                      }

-- | Renvoie les identifiants des unités par ordre d'initiative (i.e. : la
-- première unité qui doit jouer est la première unité de la liste).
initiative :: Board -> [UnitKey]
initiative board = bInitiative board

-- | Renvoie les unités du plateau de jeu.
allUnits :: Board -> XsMap CellKey UnitKey Faction Unit
allUnits b = bXsMap b

-- | Renvoie une unité du plateau de jeu.
getUnit :: Board -> UnitKey -> Unit
getUnit board ukey = case lookupKey ukey (bXsMap board) of
  Just u -> u
  Nothing -> error "malformed board" -- HACK

-- | Renvoie la distance à vol d'oiseau entre deux unités.
getDist :: Atlas -> Board -> UnitKey -> UnitKey -> Int
getDist atlas board x y = dist gsize x' y'
  where
    gsize = gridSize atlas
    x' = case lookupKey x (bXsMap board) of
      Just u -> location u
      Nothing -> error "malformed board" -- HACK
    y' = case lookupKey y (bXsMap board) of
      Just u -> location u
      Nothing -> error "malformed board" -- HACK

-- | Supprime une unité du plateau de jeu.
removeUnit :: UnitKey -> Board -> Board
removeUnit uk board = board { bXsMap = xs'
                            , bInitiative = init'
                            }
  where
    xs' = deleteKey uk (bXsMap board)
    init' = filter (\ k -> k /= uk) (bInitiative board) -- Inefficace

-- | Diminue la force d'une unité sur le plateau de jeu.
decrStrength :: UnitKey -> Double -> Board -> Board
decrStrength uk ds board = case lookupKey uk (bXsMap board) of
  Nothing -> board
  Just u -> if s' > 0
            then board { bXsMap = insertKey uk u' (bXsMap board) }
            else removeUnit uk board
    where
      s' = (strength u) - ds
      u' = u { strength_ = s' }

-- | Renvoie la position d'une unité sur le plateau de jeu.
getPosition :: Board -> UnitKey -> Position
getPosition board ukey = case lookupKey ukey (bXsMap board) of
  Just u -> position_ u
  Nothing -> error "malformed board" -- HACK

-- | Change la position d'une unité sur le plateau de jeu.
setPosition :: UnitKey -> Position -> Board -> Board
setPosition ukey pos board = board { bXsMap = xs' }
  where
    xs = bXsMap board
    u = case lookupKey ukey xs of
      Just u_ -> u_
      Nothing -> error "malformed board" -- HACK
    u' = u { position_ = pos }
    xs' = insertKey ukey u' xs

-- | Renvoie l'emplacement d'une unité du plateau de jeu.
getLocation :: Board -> UnitKey -> CellKey
getLocation board ukey = case lookupKey ukey (bXsMap board) of
  Just u -> location u
  Nothing -> error "malformed board" -- HACK

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
getCell atlas board ck = Cell { cellParams = params
                              , cellContent_ = content
                              , cellKey_ = ck
                              }
  where
    params = getHex atlas ck
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
    maxCapacity = capacity_ (getHex atlas ck)
    bunits = lookupLocationContent ck (bXsMap board)

    go :: Unit -> Double -> Double
    go u c = if faction u == f
             then c - (strength u)
             else c

-- | Renvoie la nature du terrain sur la case donnée.
tile' :: Atlas -> CellKey -> Tile
tile' atlas ck = tile_ (getHex atlas ck)

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
