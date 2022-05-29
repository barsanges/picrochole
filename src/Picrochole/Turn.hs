{- |
   Module      : Picrochole.Turn
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Résout un tour de jeu.
-}

module Picrochole.Turn
  ( turn
  ) where

import Data.Foldable ( maximumBy )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( isNothing )
import Data.Set ( Set )
import qualified Data.Set as S
import Picrochole.Board
import Picrochole.Data.Base

-- | Résout un tour de jeu.
turn :: Map UnitKey [CellKey] -> Board -> Board
turn actions board = fight (bombing (encirclement (movement actions board)))

-- | Résout la phase de mouvement.
movement :: Map UnitKey [CellKey] -> Board -> Board
movement actions board = foldr go board (initiative board)
  where

    go :: UnitKey -> Board -> Board
    go ukey b0 = goo 1 action b0
      where

        action = M.findWithDefault [] ukey actions

        goo :: Double -> [CellKey] -> Board -> Board
        goo ds keys b = case keys of
          [] -> setPosition ukey pos' b
          (y:ys) -> if isNothing (progress pos')
                       && (next b unit x y)
                    then goo ds' ys b'
                    else setPosition ukey pos' b
            where
              f = faction unit
              x = currentCell pos
              new = Position { currentCell = y, progress = Just 0 }
              b' = setMarker f x (setPosition ukey new b)

          where
            unit = getUnit b ukey
            pos = getPosition b ukey
            t = tile' b (currentCell pos)
            (ds', pos') = fwd t unit (ds, pos)

-- | Fait avancer l'unité sur sa case.
fwd :: Tile -> Unit -> (Double, Position) -> (Double, Position)
fwd t unit (ds, pos) = case progress pos of
  Nothing -> (ds, pos)
  Just dx -> (ds', pos')
    where
      v = case (t, kind unit) of
        (Road, Cavalery) -> 2
        (Road, Infantery) -> 1
        (Road, Artillery) -> 1
        (Land, Cavalery) -> 1
        (Land, Infantery) -> 0.5
        (Land, Artillery) -> 0.25
        (Water, _) -> 0
      ds' = max 0 (ds - ((1 - dx) / v))
      pos' = if (dx + ds * v) < 1
             then pos { progress = Just (dx + ds * v) }
             else pos { progress = Nothing }

-- | Indique si l'unité peut quitter la cellule `x` pour la cellule `y`.
next :: Board -> Unit -> CellKey -> CellKey -> Bool
next board unit x y = (strength unit <= capacityLeft board (faction unit) y)
                      && (touch x y)
                      && ((not $ isContested board x)
                          || (getMarker board y == Just f)
                          || (hasUnit board f y))
  where
    f = faction unit

-- | Résout la phase d'encerclement.
encirclement :: Board -> Board
encirclement board = removeSurrounded Red redsSurrounded
                     (removeSurrounded Blue bluesSurrounded board)
  where
    bluesSurrounded = allSurrounded Blue
    redsSurrounded = allSurrounded Red

    ravel :: Foldable t => t (Set CellKey) -> Set CellKey
    ravel xs = foldr S.union S.empty xs

    allSurrounded :: Faction -> Set CellKey
    allSurrounded f = ravel (filter (surrounded board (opponent f)) locs)
      where
        locs = connex (getLocations board f)

    removeSurrounded :: Faction -> Set CellKey -> Board -> Board
    removeSurrounded f xs b = foldr (removeFaction f) b xs

-- | Regroupe les cases par ensembles connexes.
connex :: Set CellKey -> [Set CellKey]
connex keys = foldr go [] keys
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
        goo (xs:xss) = if touchFold k xs
                       then (S.union xs ins, outs)
                       else (ins, xs:outs)
          where
            (ins, outs) = goo xss

-- | Indique si la case est adjacente à un groupe de cases donné.
touchFold :: Foldable t => CellKey -> t CellKey -> Bool
touchFold x ys = any (touch x) ys

-- | Indique si l'ensemble de cases `ks` est encerclé par la faction `f`.
surrounded :: Board -> Faction -> Set CellKey -> Bool
surrounded board f ks = all go edge
  where
    edge = border board ks
    go :: CellKey -> Bool
    go key = (tile cell == Water) || (not (null (getOpponents f cell)))
      where
        cell = getCell board key

-- | Renvoie les identifiants des cases qui entourent les cases données.
border :: Board -> Set CellKey -> Set CellKey
border board inside = surroundings `S.difference` inside
  where
    surroundings = foldr go S.empty inside
    go :: CellKey -> Set CellKey -> Set CellKey
    go k ks = S.union (S.fromList $ getDiskKeys board k 1) ks

-- | Résout la phase de bombardement.
bombing :: Board -> Board
bombing board = foldr go board (initiative board)
  where
    go :: UnitKey -> Board -> Board
    go key b = case kind unit of
      Artillery -> wound damage g target b
      _ -> b
      where
        loc = getLocation b key
        unit = getUnit b key
        f = faction unit
        g = opponent f
        damage = (strength unit) / 15

        disk = getDisk b loc 1
        target = getStrongestOpponent f disk

-- | Résout la phase de combat.
fight :: Board -> Board
fight board = foldr go board (getContested board)
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
wound damage f cell board = foldr go board targets -- FIXME : cela ne respecte pas la spéc !
  where
    targets = getFaction f cell
    total = totalStrength targets

    go :: Unit -> Board -> Board
    go u b = decrStrength (unitKey u) (damage / total) b
