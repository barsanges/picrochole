{- |
   Module      : Picrochole.Engine.Reporting
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Effectue la remontée d'informations entre les unités et leurs état-majors.
-}

module Picrochole.Engine.Reporting
  ( reporting
  ) where

import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as M

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Cell
import Picrochole.Data.Reports
import Picrochole.Data.Units

-- | Effectue la remontée d'informations entre les unités et leurs état-majors.
reporting :: Atlas
          -> [UnitKey]
          -> TurnCount
          -> (Faction -> UnitKey)
          -> Units
          -> Register Report
          -> Register Report
reporting atlas initiative tcount getHQ xs reports = foldl' (flip go) reports initiative
  where
    go :: UnitKey -> Register Report -> Register Report
    go ukey r = case fmap (getHQ . faction) (lookupKey ukey xs) of
      Nothing -> r
      Just hq -> case mkReport atlas xs ukey of
        Nothing -> r
        Just report -> if d' <= 3 * (tcount - prev) || prev == 0 || ukey == hq
                       then send h report r
                       else r
            where
              prev = case (lastSent r ukey hq) of
                Just msg -> sent (header msg)
                Nothing -> 0
              d = getDist atlas xs ukey hq
              d' = fromMaybe (maxBound :: Int) d
              h = mkHeader tcount ukey hq d

-- | Construit le rapport d'une unité à son état-major.
mkReport :: Atlas -> Units -> UnitKey -> Maybe Report
mkReport atlas xs ukey = case lookupKey ukey xs of
  Nothing -> Nothing
  Just unit -> Just report
    where
      ckey = location unit
      radius = if isContested xs ckey
               then 1
               else case kind unit of
                      Cavalry -> 4
                      Infantry -> 2
                      Artillery -> 2
      disk = getDisk atlas xs ckey radius
      report = foldr go M.empty disk

      go :: Cell -> Report -> Report
      go c r = M.insert (cellKey c) (cellContent c) r
