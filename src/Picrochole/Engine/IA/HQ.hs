{- |
   Module      : Picrochole.Engine.IA.HQ
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Envoie des ordres aux subordonnés.
-}

module Picrochole.Engine.IA.HQ
  ( schedule
  ) where

import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Set as S

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Cell
import Picrochole.Data.Orders
import Picrochole.Data.Plan
import Picrochole.Data.Reports
import Picrochole.Data.Units

-- | Information associée à une case du plateau.
data InfoCell = InfoCell { cellContent_ :: CellContent
                         , date_ :: TurnCount
                         }
  deriving Show

-- | Dernières informations dont dispose le QG. Cela s'approche d'une
-- photographie "en temps réel" du conflit.
type Info = Map CellKey InfoCell

-- | Envoie des ordres aux subordonnés.
schedule :: Atlas
         -> TurnCount
         -> Units
         -> UnitKey
         -> Faction
         -> Int
         -> Plan
         -> Register Report
         -> Register Order
         -> Register Order
schedule atlas tcount units ukey f limit plan rreg oreg = oreg'
  where
    gsize = gridSize atlas
    info = mkInfo tcount ukey limit rreg
    orders = assign gsize f plan info
    oreg' = command atlas tcount units ukey orders oreg

-- | Construit une image du conflit avec les dernières informations disponibles.
mkInfo :: TurnCount -> UnitKey -> Int -> Register Report -> Info
mkInfo tcount ukey limit reg = foldr f M.empty reports

  where

    reports = lastReceived' tcount reg ukey

    select :: InfoCell -> InfoCell -> InfoCell
    select a b = if (date_ a) < (date_ b)
                 then b
                 else a

    f :: Msg Report -> Info -> Info
    f msg i0 = if (sent (header msg)) >= (tcount - limit)
               then M.foldrWithKey g i0 (content msg)
               else i0
      where
        g :: CellKey -> CellContent -> Info -> Info
        g ckey inner info = M.insertWith select ckey icell info
          where
            icell = InfoCell { cellContent_ = inner, date_ = sent (header msg) }

-- | Affecte un objectif à chaque unité.
assign :: GridSize -> Faction -> Plan -> Info -> Map UnitKey CellKey
assign gsize f plan info = res
  where

    -- Les unités qui ont été appelées pour des objectifs :
    targets = foldl' go M.empty (objectives plan)

    -- Les unités de la réserve qui n'ont pas été appelées :
    left = S.difference (reserve plan) (M.keysSet targets)

    -- L'ensemble des ordres :
    res = foldr (insertMissing (concentration plan)) targets left

    go :: Map UnitKey CellKey
       -> Objective
       -> Map UnitKey CellKey
    go tmp obj = foldr (insertMissing t) tmp called
      where
        t = target obj
        called = case assess gsize f info t of
                  None -> assigned obj
                  AtRisk -> S.union (assigned obj) (reinforcements obj)

-- | Insère un couple `(k, x)` dans le dictionnaire `mp` si et seulement si
-- celui ne contient pas déjà la clef `k`.
insertMissing :: Ord k => a -> k -> Map k a -> Map k a
insertMissing x k mp = M.insertWith (\ _ old -> old) k x mp

-- | Evalue le danger associé à un objectif.
assess :: GridSize -> Faction -> Info -> CellKey -> ThreatLevel
assess gsize f info obj = if any go surroundings
                          then AtRisk
                          else None

  where

    surroundings = diskKeys gsize obj 2

    go :: CellKey -> Bool
    go ckey = case M.lookup ckey info of
      Nothing -> False
      Just c -> (not . null) (getOpponents' f (cellContent_ c))

-- | Indique à chaque subordonné son objectif.
command :: Atlas
        -> TurnCount
        -> Units
        -> UnitKey
        -> Map UnitKey CellKey
        -> Register Order
        -> Register Order
command atlas tcount units hq orders reg = M.foldrWithKey go reg orders
  where
    go :: UnitKey -> CellKey -> Register Order -> Register Order
    go ukey obj r = send h obj r
      where
        d = getDist atlas units ukey hq
        h = mkHeader tcount hq ukey d
