{- |
   Module      : Picrochole.Engine.Turn
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Résout un tour de jeu.
-}

module Picrochole.Engine.Turn
  ( turn
  , movement
  , encirclement
  , bombing
  , fight
  ) where

import Data.Foldable ( maximumBy )
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( isNothing )
import Data.Set ( Set )
import qualified Data.Set as S

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Cell
import Picrochole.Data.Units

-- | Résout un tour de jeu.
turn :: Atlas -> [UnitKey] -> Map UnitKey [CellKey] -> Units -> Units
turn atlas initiative actions units = fight atlas
                                      $ bombing atlas initiative
                                      $ encirclement atlas
                                      $ movement atlas initiative actions units

-- | Résout la phase de mouvement.
movement :: Atlas -> [UnitKey] -> Map UnitKey [CellKey] -> Units -> Units
movement atlas initiative actions units = foldl' (flip go) units initiative
  where

    go :: UnitKey -> Units -> Units
    go ukey u0 = goo 1 action u0
      where

        action = M.findWithDefault [] ukey actions

        goo :: Double -> [CellKey] -> Units -> Units
        goo ds keys u = case lookupKey ukey u of
          Nothing -> u
          Just unit -> case keys of
            [] -> setPosition ukey pos' u
            (y:ys) -> if isNothing (currentProgress pos')
                         && (next atlas u unit x y)
                      then goo ds' ys u'
                      else setPosition ukey pos' u
              where
                f = faction unit
                x = currentCell pos
                new = Position { currentCell = y, currentProgress = Just 0 }
                u' = insertToken x f (setPosition ukey new u)

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
next :: Atlas -> Units -> Unit -> CellKey -> CellKey -> Bool
next atlas units unit x y = (strength unit <= capacityLeft atlas units (faction unit) y)
                            && (touch (gridSize atlas) x y)
                            && ((not $ isContested units x)
                                || (lookupLocationToken y units == Just f)
                                || (hasUnit units f y))
  where
    f = faction unit

-- | Résout la phase d'encerclement.
encirclement :: Atlas -> Units -> Units
encirclement atlas units = removeSurrounded Red redsSurrounded
                           $ removeSurrounded Blue bluesSurrounded units
  where

    bluesSurrounded = allSurrounded Blue
    redsSurrounded = allSurrounded Red

    ravel :: Foldable t => t (Set CellKey) -> Set CellKey
    ravel xs = foldr S.union S.empty xs

    allSurrounded :: Faction -> Set CellKey
    allSurrounded f = ravel (filter (surrounded atlas units f) locs)
      where
        locs = connex (gridSize atlas) (locations units f)

    removeSurrounded :: Faction -> Set CellKey -> Units -> Units
    removeSurrounded f xs u = foldr (removeFaction f) u xs

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

-- | Indique si l'ensemble de cases `ks` est encerclé par la faction opposée
-- à `f`.
surrounded :: Atlas -> Units -> Faction -> Set CellKey -> Bool
surrounded atlas units f ks = all go edge
  where
    edge = border atlas ks
    go :: CellKey -> Bool
    go key = (topography' atlas key == Water) || (not (null (getOpponents f cell)))
      where
        cell = getCell atlas units key

-- | Renvoie les identifiants des cases qui entourent les cases données.
border :: Atlas -> Set CellKey -> Set CellKey
border atlas inside = surroundings `S.difference` inside
  where
    surroundings = foldr go S.empty inside
    go :: CellKey -> Set CellKey -> Set CellKey
    go k ks = S.union (S.fromList $ getDiskKeys atlas k 1) ks

-- | Résout la phase de bombardement.
bombing :: Atlas -> [UnitKey] -> Units -> Units
bombing atlas initiative units = foldl' (flip go) units initiative
  where
    go :: UnitKey -> Units -> Units
    go key u = case lookupKey key u of
      Nothing -> u
      Just unit -> case kind unit of
        Artillery -> wound damage g target u
        _ -> u
        where
          loc = location unit
          f = faction unit
          g = opponent f
          damage = (strength unit) / 15

          disk = getDisk atlas u loc 1
          target = getStrongestOpponent f disk

-- | Résout la phase de combat.
fight :: Atlas -> Units -> Units
fight atlas units = foldr go units (getContested atlas units)
  where
    go :: Cell -> Units -> Units
    go cell u = wound damageToRed Red cell (wound damageToBlue Blue cell u)
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
      Cavalry -> strength u
      Infantry -> strength u
      Artillery -> 0

-- | Renvoie la cellule contenant le plus fort groupe d'adversaires.
getStrongestOpponent :: Foldable t => Faction -> t Cell -> Cell
getStrongestOpponent f cs = maximumBy (\ x y -> compare (go x) (go y)) cs
  where
    go x = totalStrength (getOpponents f x)

-- | Répartit des dommages entre les unités de la faction indiquée.
wound :: Double -> Faction -> Cell -> Units -> Units
wound damage f cell units = case getStrongest f cell of
  Just u -> decrStrength (unitKey u) damage units
  Nothing -> units
