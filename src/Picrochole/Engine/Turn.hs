{- |
   Module      : Picrochole.Engine.Turn
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Résout un tour de jeu.
-}

module Picrochole.Engine.Turn
  ( turn
  ) where

import Data.Foldable ( maximumBy )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( isNothing )
import Data.Set ( Set )
import qualified Data.Set as S
import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Board

-- | Résout un tour de jeu.
turn :: Atlas -> [UnitKey] -> Map UnitKey [CellKey] -> Board -> Board
turn atlas initiative actions board = fight atlas
                                      $ bombing atlas initiative
                                      $ encirclement atlas
                                      $ movement atlas initiative actions board

-- | Résout la phase de mouvement.
movement :: Atlas -> [UnitKey] -> Map UnitKey [CellKey] -> Board -> Board
movement atlas initiative actions board = foldr go board initiative
  where

    go :: UnitKey -> Board -> Board
    go ukey b0 = goo 1 action b0
      where

        action = M.findWithDefault [] ukey actions

        goo :: Double -> [CellKey] -> Board -> Board
        goo ds keys b = case getUnit b ukey of
          Nothing -> b
          Just unit -> case keys of
            [] -> setPosition ukey pos' b
            (y:ys) -> if isNothing (currentProgress pos')
                         && (next atlas b unit x y)
                      then goo ds' ys b'
                      else setPosition ukey pos' b
              where
                f = faction unit
                x = currentCell pos
                new = Position { currentCell = y, currentProgress = Just 0 }
                b' = setMarker f x (setPosition ukey new b)

            where
              pos = position unit
              t = topography' atlas (currentCell pos)
              (ds', pos') = fwd t unit (ds, pos)

-- | Fait avancer l'unité sur sa case.
fwd :: Topography -> Unit -> (Double, Position) -> (Double, Position)
fwd t unit (ds, pos) = case currentProgress pos of
  Nothing -> (ds, pos)
  Just dx -> (ds', pos')
    where
      v = speed (kind unit) t
      ds' = max 0 (ds - ((1 - dx) / v))
      pos' = if (dx + ds * v) < 1
             then pos { currentProgress = Just (dx + ds * v) }
             else pos { currentProgress = Nothing }

-- | Indique si l'unité peut quitter la cellule `x` pour la cellule `y`.
next :: Atlas -> Board -> Unit -> CellKey -> CellKey -> Bool
next atlas board unit x y = (strength unit <= capacityLeft atlas board (faction unit) y)
                            && (touch (gridSize atlas) x y)
                            && ((not $ isContested board x)
                                || (getMarker board y == Just f)
                                || (hasUnit board f y))
  where
    f = faction unit

-- | Résout la phase d'encerclement.
encirclement :: Atlas -> Board -> Board
encirclement atlas board = removeSurrounded Red redsSurrounded
                           $ removeSurrounded Blue bluesSurrounded board
  where

    bluesSurrounded = allSurrounded Blue
    redsSurrounded = allSurrounded Red

    ravel :: Foldable t => t (Set CellKey) -> Set CellKey
    ravel xs = foldr S.union S.empty xs

    allSurrounded :: Faction -> Set CellKey
    allSurrounded f = ravel (filter (surrounded atlas board (opponent f)) locs)
      where
        locs = connex (gridSize atlas) (getLocations board f)

    removeSurrounded :: Faction -> Set CellKey -> Board -> Board
    removeSurrounded f xs b = foldr (removeFaction f) b xs

-- | Regroupe les cases par ensembles connexes.
connex :: GridSize -> Set CellKey -> [Set CellKey]
connex gsize keys = foldr go [] keys
  where

    -- Ajoute la case `k` à un ensemble de cases connexes (ce qui peut
    -- occasionner des fusions d'ensembles qui deviennent connexes), ou
    -- crée un ensemble dédié.
    go :: CellKey -> [Set CellKey] -> [Set CellKey]
    go k ks = (S.insert k ok):noks
      where
        (ok, noks) = goo ks

        -- Partitionne la liste entre les éléments connexes à `k` et ceux
        -- qui ne le sont pas.
        goo :: [Set CellKey] -> (Set CellKey, [Set CellKey])
        goo [] = (S.empty, [])
        goo (xs:xss) = if touchFold gsize k xs
                       then (S.union xs ins, outs)
                       else (ins, xs:outs)
          where
            (ins, outs) = goo xss

-- | Indique si la case est adjacente à un groupe de cases donné.
touchFold :: Foldable t => GridSize -> CellKey -> t CellKey -> Bool
touchFold gsize x ys = any (touch gsize x) ys

-- | Indique si l'ensemble de cases `ks` est encerclé par la faction `f`.
surrounded :: Atlas -> Board -> Faction -> Set CellKey -> Bool
surrounded atlas board f ks = all go edge
  where
    edge = border atlas ks
    go :: CellKey -> Bool
    go key = (topography' atlas key == Water) || (not (null (getOpponents f cell)))
      where
        cell = getCell atlas board key

-- | Renvoie les identifiants des cases qui entourent les cases données.
border :: Atlas -> Set CellKey -> Set CellKey
border atlas inside = surroundings `S.difference` inside
  where
    surroundings = foldr go S.empty inside
    go :: CellKey -> Set CellKey -> Set CellKey
    go k ks = S.union (S.fromList $ getDiskKeys atlas k 1) ks

-- | Résout la phase de bombardement.
bombing :: Atlas -> [UnitKey] -> Board -> Board
bombing atlas initiative board = foldr go board initiative
  where
    go :: UnitKey -> Board -> Board
    go key b = case getUnit b key of
      Nothing -> b
      Just unit -> case kind unit of
        Artillery -> wound damage g target b
        _ -> b
        where
          loc = location unit
          f = faction unit
          g = opponent f
          damage = (strength unit) / 15

          disk = getDisk atlas b loc 1
          target = getStrongestOpponent f disk

-- | Résout la phase de combat.
fight :: Atlas -> Board -> Board
fight atlas board = foldr go board (getContested atlas board)
  where
    go :: Cell -> Board -> Board
    go cell b = wound damageToRed Red cell (wound damageToBlue Blue cell b)
      where
        damageToRed = (totalStrength' (getBlues cell)) / 30
        damageToBlue = (totalStrength' (getReds cell)) / 30

-- | Renvoie la force totale d'un ensemble d'unités, toutes armes confondues.
totalStrength :: Foldable t => t Unit -> Double
totalStrength units = foldr (\ u x -> x + strength u)  0 units

-- | Renvoie la force totale d'un ensemble d'unités, en ne comptant que la
-- cavalerie et l'infanterie.
totalStrength' :: Foldable t => t Unit -> Double
totalStrength' units = foldr (\ u x -> x + go u)  0 units
  where
    go :: Unit -> Double
    go u = case kind u of
      Cavalery -> strength u
      Infantery -> strength u
      Artillery -> 0

-- | Renvoie la cellule contenant le plus fort groupe d'adversaires.
getStrongestOpponent :: Foldable t => Faction -> t Cell -> Cell
getStrongestOpponent f cs = maximumBy (\ x y -> compare (go x) (go y)) cs
  where
    go x = totalStrength (getOpponents f x)

-- | Répartit des dommages entre les unités de la faction indiquée.
wound :: Double -> Faction -> Cell -> Board -> Board
wound damage f cell board = case getStrongest f cell of
  Just u -> decrStrength (unitKey u) damage board
  Nothing -> board
