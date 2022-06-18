{- |
   Module      : Picrochole.Engine.Reporting
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Effectue la remontée d'informations entre les unités et leurs état-majors.
-}

module Picrochole.Engine.Reporting
  ( reporting
  ) where

import Picrochole.Data.Base
import Picrochole.Data.Board
import Picrochole.Data.Mail

-- | Effectue la remontée d'informations entre les unités et leurs état-majors.
reporting :: TurnCount -> (Faction -> UnitKey) -> Board -> Post -> Post
reporting tCount getHQ board post = foldr go post (initiative board)
  where
    go :: UnitKey -> Post -> Post
    go ukey p = if d <= 3 * (tCount - prev) || prev == 0
                then sendReport header report p
                else p
      where
        hq = getHQ (faction $ getUnit board ukey)
        d = getDist board ukey hq
        prev = case (dateLastReportSent p ukey hq) of
          Just t -> t
          Nothing -> 0
        header = mkHeader tCount ukey hq d
        report = mkReport board ukey

-- | Construit le rapport d'une unité à son état-major.
mkReport :: Board -> UnitKey -> Report
mkReport board ukey = cells
  where
    ckey = getLocation board ukey
    radius = if isContested board ckey
             then 1
             else case undefined of
                    Cavalery -> 4
                    Infantery -> 2
                    Artillery -> 2
    cells = getDisk board ckey radius
