{- |
   Module      : Picrochole.Board
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Le plateau de jeu et les unités des deux camps.
-}

module Picrochole.Board
  ( Unit(..)
  , Position(..)
  , unitKey
  , faction
  , kind
  , CellKey
  , Cell
  , cellKey
  , tile
  , capacity
  , getBlues
  , getReds
  , getFaction
  , getOpponents
  , setBlues
  , setReds
  , Board
  , touch
  , initiative
  , getUnit
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

import Data.List ( partition )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Picrochole.Data.Base
import Picrochole.Utils.XsMap

-- | Paramètres immuables d'une unité.
data UnitParams = UP { unitKey_ :: UnitKey
                     , faction_ :: Faction
                     , kind_ :: UnitKind
                     }
  deriving Show

-- | Une unité du plateau de jeu.
data Unit = Unit { unitParams :: UnitParams
                 , strength :: Double
                 }
  deriving Show

-- | Position d'une unité sur le plateau de jeu.
data Position = Position { currentCell :: CellKey
                         , progress :: Maybe Double
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

-- | Identifiant unique d'une case.
data CellKey = CK Int Int
  deriving (Eq, Ord, Show)

-- | Paramètres immuables d'une case du plateau de jeu.
data CellParams = CP { cellKey_ :: CellKey
                     , tile_ :: Tile
                     , capacity_ :: Double
                     }
  deriving Show

-- | Contenu d'une cellule.
type CellContent = Either Faction ([Unit], [Unit])

-- | Une case du plateau de jeu.
data Cell = Cell { cellParams :: CellParams
                 , cellContent :: CellContent
                 }
  deriving Show

-- | Renvoie l'identifiant de la case.
cellKey :: Cell -> CellKey
cellKey c = cellKey_ (cellParams c)

-- | Renvoie la nature du terrain sur la case.
tile :: Cell -> Tile
tile c = tile_ (cellParams c)

-- | Renvoie la capacité maximale (par camp) de la case.
capacity :: Cell -> Double
capacity c = capacity_ (cellParams c)

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

-- | Remplace les unités bleues de la case par la liste d'unités
-- fournies. La faction de ces unités n'est pas vérifiée.
setBlues :: [Unit] -> Cell -> Cell
setBlues xs c = c { cellContent = new }
  where
    new = case cellContent c of
      Left f -> if null xs
                then Left f
                else Right (xs, [])
      Right (_, ys) -> Right (xs, ys)

-- | Remplace les unités rouges de la case par la liste d'unités
-- fournies. La faction de ces unités n'est pas vérifiée.
setReds :: [Unit] -> Cell -> Cell
setReds ys c = c { cellContent = new }
  where
    new = case cellContent c of
      Left f -> if null ys
                then Left f
                else Right ([], ys)
      Right (xs, _) -> Right (xs, ys)
-- FIXME : faire le ménage dans ces fonctions, toutes ne semblent pas
-- utilisées.

-- | Une unité stockée dans une structure de type `Board`.
data BUnit k = BU { unit_ :: Unit
                  , currentCell_ :: k
                  , progress_ :: Maybe Double
                  }
  deriving Show
-- FIXME : devoir faire intervenir `k` de cette manière n'est vraiment pas
-- naturel !

instance HasLocation BUnit where
  location = currentCell_

-- | Le plateau de jeu avec l'ensemble des unités des deux camps.
data Board = Board { bCellParams :: Vector CellParams
                   , bXsMap :: XsMap CellKey UnitKey BUnit
                   , bMarkers :: Map CellKey Faction
                   , bInitiative :: [UnitKey]
                   , ncols :: Int
                   , nrows :: Int
                   } -- TODO
  deriving Show

-- TODO :
--    * un vecteur des cellules, avec leur Tile et leur capacité
--    * une structure avec les données des unités
--    * l'emplacement des unités (sachant qu'il faut pouvoir renvoyer
--      toutes les unités d'un camp)
--    * l'emplacement des marqueurs
--    * l'initiative

-- Il faut pouvoir donner les positions des unités par clef, mais aussi
-- par lieu.

-- Dans les cas d'usage auxquels penser : quand on modifie le contenu
-- d'une cellule et que cela supprime / ajoute une unité, il faut
-- idéalement supprimer toute référence à celle-ci.

-- FIXME : avoir une structure spécifique pour une grille hexagonale ?

-- | Convertit la clef en un indice entier permettant de requêter le vecteur des
-- cases.
toIntCoord :: Board -> CellKey -> Maybe Int
toIntCoord board (CK x y) = if withinBoard board (CK x y)
                            then Just (y * (ncols board) + x)
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

-- | Indique si la case se situe sur le plateau.
withinBoard :: Board -> CellKey -> Bool
withinBoard b (CK x y) = 0 <= x && x < (ncols b) && 0 <= y && y < (nrows b)

-- | Indique si les cases `x` et `y` sont adjacentes.
touch :: CellKey -> CellKey -> Bool
touch x y = (dist x y) == 1

-- | Renvoie les identifiants des unités par ordre d'initiative (i.e. : la
-- première unité qui doit jouer est la première unité de la liste).
initiative :: Board -> [UnitKey]
initiative board = bInitiative board

-- | Renvoie une unité du plateau de jeu.
getUnit :: Board -> UnitKey -> Unit
getUnit board ukey = case lookupKey ukey (bXsMap board) of
  Just bu -> unit_ bu
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
  Just bu -> if s' > 0
             then board { bXsMap = insertKey uk bu' (bXsMap board) }
             else removeUnit uk board
    where
      s' = (strength (unit_ bu)) - ds
      u' = (unit_ bu) { strength = s' }
      bu' = bu { unit_ = u' }

-- TODO : si une case devient libre parce que la dernière unité dessus a
-- été détruite, cela ne nécessite aucune action regardant les marqueurs
-- (le contenu de la case est bien `Right ([], [])`).

-- | Renvoie la position d'une unité sur le plateau de jeu.
getPosition :: Board -> UnitKey -> Position
getPosition board ukey = case lookupKey ukey (bXsMap board) of
  Just bu -> Position { currentCell = currentCell_ bu
                      , progress = progress_ bu
                      }
  Nothing -> error "malformed board" -- HACK

-- | Change la position d'une unité sur le plateau de jeu.
setPosition :: UnitKey -> Position -> Board -> Board
setPosition ukey pos board = board { bXsMap = xs' }
  where
    xs = bXsMap board
    bu = case lookupKey ukey xs of
      Just bu_ -> bu_
      Nothing -> error "malformed board" -- HACK
    bu' = bu { currentCell_ = currentCell pos
             , progress_ = progress pos
             }
    xs' = insertKey ukey bu' xs

-- | Renvoie l'emplacement d'une unité du plateau de jeu.
getLocation :: Board -> UnitKey -> CellKey
getLocation board ukey = case lookupKey ukey (bXsMap board) of
  Just bu -> currentCell_ bu
  Nothing -> error "malformed board" -- HACK

-- | Renvoie l'emplacement de toutes les unités d'un camp.
getLocations :: Board -> Faction -> Set CellKey
getLocations board f = foldXsMap go S.empty (bXsMap board)
  where
    go :: BUnit CellKey -> Set CellKey -> Set CellKey
    go bu s = if faction (unit_ bu) == f
              then S.insert (currentCell_ bu) s
              else s

-- | Renvoie une case du plateau de jeu.
getCell :: Board -> CellKey -> Cell
getCell board ck = Cell { cellParams = params
                        , cellContent = content
                        }
  where
    params = case toIntCoord board ck of
                   Just idx -> ((bCellParams board) V.! idx)
                   Nothing -> error ("unknown cell " ++ show ck)
    bunits = lookupLocation ck (bXsMap board)
    units = fmap unit_ bunits
    (blues, reds) = partition (\ x -> faction x == Blue) units
    mmarker = M.lookup ck (bMarkers board)
    content = case (mmarker, bunits) of
      (Just f, []) -> Left f
      _ -> Right (blues, reds)

-- | Renvoie les identifiants d'un disque de cases du plateau de jeu, dont le
-- centre est la case indiquée.
getDiskKeys :: Board -> CellKey -> Int -> [CellKey]
getDiskKeys board ck radius = filter (withinBoard board) allKeys
  where
    (q, r, s) = toHexCoord ck
    allKeys = [fromHexCoord (q + dq, r + dr, s - (dq + dr))
               | dq <- [-radius..radius]
               , dr <- [(max (-radius) (-dq - radius))..(max radius (dq + radius))]
              ]
-- FIXME : dépend de la grille hexagonale.

-- | Renvoie un disque de cases du plateau de jeu, dont le centre est la case
-- indiquée.
getDisk :: Board -> CellKey -> Int -> [Cell]
getDisk board ck radius = fmap (getCell board) keys
  where
    keys = getDiskKeys board ck radius

-- | Renvoie toutes les cases du plateau de jeu qui contiennent des unités
-- des deux camps.
getContested :: Board -> [Cell]
getContested board = fmap (getCell board) contested
  where
    blues = getLocations board Blue
    reds = getLocations board Red
    contested = S.toList (S.intersection blues reds)

-- | Supprime toutes les unités d'un camp sur la case donnée.
removeFaction :: Faction -> CellKey -> Board -> Board
removeFaction f ck board = foldr go board bunits
  where
    bunits = lookupLocation ck (bXsMap board)

    go :: BUnit CellKey -> Board -> Board
    go bu b = if faction (unit_ bu) == f
              then removeUnit (unitKey (unit_ bu)) b
              else b

-- | Renvoie la capacité d'accueil restante de la case donnée.
capacityLeft :: Board -> Faction -> CellKey -> Double
capacityLeft board f ck = foldr go maxCapacity bunits
  where
    maxCapacity = case toIntCoord board ck of
                   Just idx -> capacity_ (((bCellParams board) V.! idx))
                   Nothing -> error ("unknown cell " ++ show ck)
    bunits = lookupLocation ck (bXsMap board)

    go :: BUnit CellKey -> Double -> Double
    go bu c = if faction (unit_ bu) == f
              then c - (strength (unit_ bu))
              else c

-- | Renvoie la nature du terrain sur la case donnée.
tile' :: Board -> CellKey -> Tile
tile' board ck = case toIntCoord board ck of
                   Just idx -> tile_ (((bCellParams board) V.! idx))
                   Nothing -> error ("unknown cell " ++ show ck)

-- | Renvoie le marqueur sur la case donnée.
getMarker :: Board -> CellKey -> Maybe Faction
getMarker board ck = if null (lookupLocation ck (bXsMap board))
                     then M.lookup ck (bMarkers board)
                     else Nothing

-- | Met à jour le marqueur sur la case donnée.
setMarker :: Faction -> CellKey -> Board -> Board
setMarker f ck board = board { bMarkers = markers' }
  where
    markers' = M.insert ck f (bMarkers board)

-- | Indique si la case donnée contient une unité du camp indiqué.
hasUnit :: Board -> Faction -> CellKey -> Bool
hasUnit board f x = any go (lookupLocation x (bXsMap board))
  where
    go :: BUnit CellKey -> Bool
    go bu = (faction $ unit_ bu) == f

-- | Indique si la case donnée contient des unités des deux camps.
isContested :: Board -> CellKey -> Bool
isContested board x = (hasUnit board Blue x) && (hasUnit board Red x)
