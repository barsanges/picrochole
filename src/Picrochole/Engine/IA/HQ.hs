{- |
   Module      : Picrochole.Engine.IA.HQ
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Envoie des ordres aux subordonnés.
-}

module Picrochole.Engine.IA.HQ
  ( schedule
  ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Picrochole.Data.Base
import Picrochole.Data.Board
import Picrochole.Data.Mail
import Picrochole.Data.Plan
import Picrochole.Data.Utils.HexGrid

-- | Information associée à une case du plateau.
data InfoCell = InfoCell { cellContent_ :: CellContent
                         , date_ :: TurnCount
                         }
  deriving Show

-- | Dernières informations dont dispose le QG. Cela s'approche d'une
-- photographie "en temps réel" du conflit.
type Info = Map CellKey InfoCell

-- | Association d'une unité et d'un objectif. La relation d'égalité associée à
-- ce type ne porte que sur l'unité (i.e. : elle ignore l'objectif), de manière
-- à pouvoir définir un Set qui ne contient chaque unité qu'une fois.
data BaseOrder = BaseOrder UnitKey CellKey
  deriving Show

instance Eq BaseOrder where
  (BaseOrder x _) == (BaseOrder y _) = (x == y)

instance Ord BaseOrder where
  compare (BaseOrder x _) (BaseOrder y _) = compare x y

-- | Envoie des ordres aux subordonnés.
schedule :: TurnCount -> Board -> UnitKey -> Faction -> Int -> Plan -> Post -> Post
schedule tcount board ukey f limit plan post = post'
  where
    gsize = boardSize board
    info = mkInfo tcount ukey limit post
    new = assign gsize f plan info
    post' = send tcount board ukey new post

-- | Construit une image du conflit avec les dernières informations disponibles.
mkInfo :: TurnCount -> UnitKey -> Int -> Post -> Info
mkInfo tcount ukey limit post = foldr add M.empty infoCells

  where

    lastReports = getLastReports ukey post
    infoCells = concat (fmap toInfoCell lastReports)

    toInfoCell :: (Header, Report) -> [(CellKey, InfoCell)]
    toInfoCell (header, report) = if (sent header) >= (tcount - limit)
                                  then fmap go report
                                  else []
      where
        go :: (CellKey, CellContent) -> (CellKey, InfoCell)
        go (ckey, c) = (ckey, InfoCell { cellContent_ = c, date_ = sent header })

    add :: (CellKey, InfoCell) -> Info -> Info
    add (ckey, ic) info = M.insertWith go ckey ic info
      where
        go :: InfoCell -> InfoCell -> InfoCell
        go a b = if (date_ a) < (date_ b)
                 then b
                 else a

-- | Affecte un objectif à chaque unité.
assign :: GridSize -> Faction -> Plan -> Info -> Set BaseOrder
assign gsize f plan info = res

  where

    (_, res) = foldr go (subordinates plan, S.empty) (objectives plan)

    go :: Objective
       -> (Set UnitKey, Set BaseOrder)
       -> (Set UnitKey, Set BaseOrder)
    go obj (ukeys, tmp) = (left, tmp')
      where
        t = target obj
        called = case assess gsize f info t of
                  None -> assigned obj
                  AtRisk -> S.union (assigned obj) (reinforcements obj)
        left = S.difference ukeys called
        new = S.map (\ x -> BaseOrder x t) called
        tmp' = S.union tmp new

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
      Just c -> null (getOpponents' f (cellContent_ c))

-- | Indique à chaque subordonné son objectif.
send :: TurnCount -> Board -> UnitKey -> Set BaseOrder -> Post -> Post
send tcount board hq newOrders post = foldr go post newOrders
  where
    go :: BaseOrder -> Post -> Post
    go (BaseOrder ukey obj) p = sendOrder header obj p
      where
        d = getDist board ukey hq
        header = mkHeader tcount hq ukey d
