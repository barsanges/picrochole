{- |
   Module      : Picrochole.Engine.Reporting
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Effectue la remontée d'informations entre les unités et leurs état-majors.
-}

module Picrochole.Engine.Reporting
  ( reporting
  ) where

import qualified Data.Map as M
import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Board
import Picrochole.Data.Mail

-- | Effectue la remontée d'informations entre les unités et leurs état-majors.
reporting :: Atlas
          -> TurnCount
          -> (Faction -> UnitKey)
          -> Board
          -> Register Report
          -> Register Report
reporting atlas tcount getHQ board reports = foldr go reports (initiative board)
  where
    go :: UnitKey -> Register Report -> Register Report
    go ukey r = if d <= 3 * (tcount - prev) || prev == 0
                then send h report r
                else r
      where
        hq = getHQ (faction $ getUnit board ukey)
        d = getDist atlas board ukey hq
        prev = case (lastReceived tcount r ukey hq) of
          Just msg -> received (header msg)
          Nothing -> 0
        h = mkHeader tcount ukey hq d
        report = mkReport atlas board ukey

-- | Construit le rapport d'une unité à son état-major.
mkReport :: Atlas -> Board -> UnitKey -> Report
mkReport atlas board ukey = report
  where
    ckey = getLocation board ukey
    radius = if isContested board ckey
             then 1
             else case undefined of
                    Cavalery -> 4
                    Infantery -> 2
                    Artillery -> 2
    disk = getDisk atlas board ckey radius
    report = foldr go M.empty disk

    go :: Cell -> Report -> Report
    go c r = M.insert (cellKey c) (cellContent c) r
