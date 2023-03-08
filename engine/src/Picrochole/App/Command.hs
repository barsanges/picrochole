{- |
   Module      : Picrochole.App.Command
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Commandes élémentaires d'une partie de Picrochole.
-}

module Picrochole.App.Command
  ( run
  , order
  ) where

import System.FilePath.Posix ( (</>) )
import System.Directory

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Config
import Picrochole.Data.Orders
import Picrochole.Data.Reports
import Picrochole.Data.Units

import Picrochole.Engine.IA.HQ ( schedule )
import Picrochole.Engine.IA.PathFinding ( route )
import Picrochole.Engine.Reporting ( reporting )
import Picrochole.Engine.Turn ( turn )

import Picrochole.JSON.Pieces

-- | Résout le tour en cours.
run :: Everything -> FilePath -> IO Everything
run everything dir = do
  createDirectoryIfMissing False (dir </> "past")

  writeReports (dir </> "reports.json") reports'
  writeOrders (dir </> "orders.json") orders'
  writeCurrentTurn (dir </> "current-turn.json") tcount'

  renameFile (dir </> "current-units.json") (dir </> "past" </> ("units-"  ++ show tcount' ++ ".json"))
  writeUnits (dir </> "current-units.json") units'

  return (everything { ecurrentTurn = tcount'
                     , eorders = orders'
                     , ereports = reports'
                     , eunits = units'
                     })

  where
    atlas = eatlas everything
    config = econfig everything
    tcount = ecurrentTurn everything
    initiative = einitiative everything
    orders = eorders everything
    reports = ereports everything
    iaPlan = eplan everything
    units = eunits everything

    ia = iaFaction config
    iaHQ = getHQ config ia

    routes = route atlas initiative tcount units orders (getHQ config)
    units' = turn atlas initiative routes units
    tcount' = tcount + 1
    reports' = reporting atlas initiative tcount' (getHQ config) units' reports
    orders' = schedule atlas tcount' units' iaHQ ia iaPlan reports' orders

-- | Envoie un ordre (i.e. : une destination à atteindre) à un subordonné.
order :: Everything
      -> UnitKey
      -> Int
      -> FilePath
      -> IO (Either String Everything)
order everything ukey destination dir = do
  if isInsideGrid atlas objective
    then do
    let d = getDist atlas units ukey playerHQ
    let h = mkHeader tcount playerHQ ukey d
    let orders' = send h objective orders
    writeOrders (dir </> "orders.json") orders'
    return (Right everything { eorders = orders' })

    else pure (Left $ "the given destination ("
               ++ show destination
               ++ ") does not exist")
  where
    atlas = eatlas everything
    config = econfig everything
    tcount = ecurrentTurn everything
    orders = eorders everything
    units = eunits everything

    ia = iaFaction config
    playerHQ = getHQ config (opponent ia)
    objective = CK destination
