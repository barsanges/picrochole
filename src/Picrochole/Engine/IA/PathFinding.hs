{- |
   Module      : Picrochole.Engine.IA.PathFinding
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Détermine le chemin suivi par les unités.
-}

module Picrochole.Engine.IA.PathFinding
  ( route
  ) where

import Algorithm.Search ( dijkstra )
import Data.Map ( Map )
import qualified Data.Map as M

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Cell
import Picrochole.Data.Orders
import Picrochole.Data.Units

-- | Détermine le chemin suivi par les unités.
route :: Atlas
      -> [UnitKey]
      -> TurnCount
      -> Units
      -> Register Order
      -> (Faction -> UnitKey)
      -> Map UnitKey [CellKey]
route atlas initiative tcount xs orders getHQ = foldr go M.empty initiative
  where
    go ukey m = M.insert ukey (findPath atlas tcount xs orders getHQ ukey) m

-- | Calcule le chemin que doit suivre l'unité.
findPath :: Atlas
         -> TurnCount
         -> Units
         -> Register Order
         -> (Faction -> UnitKey)
         -> UnitKey
         -> [CellKey]
findPath atlas tcount xs orders getHQ ukey =
  case lookupKey ukey xs of
    Nothing -> []
    Just unit -> case lastReceived tcount orders ukey hq of
      Nothing -> []
      Just msg -> shortest atlas xs k size start (content msg)
      where
        start = location unit
        hq = getHQ (faction unit)
        k = kind unit
        size = strength unit

-- | Renvoie le plus court chemin entre deux emplacements, sans tenir compte
-- de l'occupation des cases, mais en tenant compte de la capacité maximale
-- des cellules.
shortest :: Atlas
         -> Units
         -> UnitKind
         -> Double
         -> CellKey
         -> CellKey
         -> [CellKey]
shortest atlas xs k size start end = case sol of
                                       Nothing -> []
                                       Just (_, path) -> path
  where
    sol = dijkstra ngb dst (\ x -> x == end) start

    ngb :: CellKey -> [CellKey]
    ngb x = fmap cellKey (filter sieve (getDisk atlas xs x 1))

    sieve :: Cell -> Bool
    sieve c = ((cellTopography c == Road) || (cellTopography c == Land))
              && (cellCapacity c >= size)

    dst :: CellKey -> CellKey -> Double
    dst x y = (weight x) + (weight y)
    -- On compte les cases de départ et d'arrivée pour éviter d'avoir un graphe
    -- orienté.

    weight :: CellKey -> Double
    weight ckey = 1 / (speed k (topography' atlas ckey))
