{- |
   Module      : Main
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Prend en charge les différentes commandes utilisées pour une partie de
Picrochole.
-}

module Main where

import Options.Applicative ( ParserInfo, (<**>) )
import qualified Options.Applicative as O
import System.FilePath.Posix ( (</>) )
import System.Directory

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

-- | Command line parser for 'turing'.
args :: ParserInfo FilePath
args = O.info ( arg <**> O.helper ) desc
  where
    arg = O.strArgument ( O.metavar "DIR"
                          <> O.help "Directory containing the current game"
                        )
    desc = O.fullDesc
           <> O.header "picrochole - Game engine for pre-napoleonic and napoleonic warfare"
           <> O.progDesc "Resolve the current turn in the given game"

-- | Résout le tour courant.
main :: IO ()
main = do
  dir <- O.execParser args
  meverything <- loadEverythingDir dir
  case meverything of
    Left m -> putStrLn m
    Right everything -> do
      let atlas = eatlas everything
      let config = econfig everything
      let tcount = ecurrentTurn everything
      let initiative = einitiative everything
      let orders = eorders everything
      let reports = ereports everything
      let iaPlan = eplan everything
      let units = eunits everything

      let ia = iaFaction config
      let iaHQ = getHQ config ia

      let routes = route atlas initiative tcount units orders (getHQ config)
      let units' = turn atlas initiative routes units
      let tcount' = tcount + 1
      let reports' = reporting atlas initiative tcount' (getHQ config) units' reports
      let orders' = schedule atlas tcount' units' iaHQ ia iaPlan reports' orders

      createDirectoryIfMissing False (dir </> "past")

      writeReports (dir </> "reports.json") reports'
      writeOrders (dir </> "orders.json") orders'
      writeCurrentTurn (dir </> "current-turn.json") tcount'

      renameFile (dir </> "current-units.json") (dir </> "past" </> ("units-"  ++ show tcount' ++ ".json"))
      writeUnits (dir </> "current-units.json") units'
