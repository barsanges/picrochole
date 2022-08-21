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
import Picrochole.Data.Board
import Picrochole.Data.Mail

-- | Détermine le chemin suivi par les unités.
route :: Atlas
      -> TurnCount
      -> Board
      -> Register Order
      -> (Faction -> UnitKey)
      -> Map UnitKey [CellKey]
route atlas tcount board orders getHQ = foldr go M.empty (initiative board)
  where
    go ukey m = M.insert ukey (findPath atlas tcount board orders getHQ ukey) m

-- | Calcule le chemin que doit suivre l'unité.
findPath :: Atlas
         -> TurnCount
         -> Board
         -> Register Order
         -> (Faction -> UnitKey)
         -> UnitKey
         -> [CellKey]
findPath atlas tcount board orders getHQ ukey = case lastReceived tcount orders ukey hq of
  Just msg -> shortest atlas board k size start (content msg)
  Nothing -> []
  where
    start = currentCell (getPosition board ukey)
    unit = getUnit board ukey
    hq = getHQ (faction unit)
    k = kind unit
    size = strength unit

-- | Renvoie le plus court chemin entre deux emplacements, sans tenir compte
-- de l'occupation des cases, mais en tenant compte de la capacité maximale
-- des cellules.
shortest :: Atlas
         -> Board
         -> UnitKind
         -> Double
         -> CellKey
         -> CellKey
         -> [CellKey]
shortest atlas board k size start end = case sol of
                                          Nothing -> []
                                          Just (_, path) -> path
  where
    sol = dijkstra ngb dst (\ x -> x == end) start

    ngb :: CellKey -> [CellKey]
    ngb x = fmap cellKey (filter sieve (getDisk atlas board x 1))

    sieve :: Cell -> Bool
    sieve c = ((tile c == Road) || (tile c == Land))
              && (capacity c >= size)

    dst :: CellKey -> CellKey -> Double
    dst x y = (weight x) + (weight y)
    -- On compte les cases de départ et d'arrivée pour éviter d'avoir un graphe
    -- orienté.

    weight :: CellKey -> Double
    weight ckey = 1 / (speed k (tile' atlas ckey))
