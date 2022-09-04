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

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Config
import Picrochole.Data.Initiative
import Picrochole.Data.Orders
import Picrochole.Data.Plan
import Picrochole.Data.Reports
import Picrochole.Data.Units

import Picrochole.Engine.IA.HQ ( schedule )
import Picrochole.Engine.IA.PathFinding ( route )
import Picrochole.Engine.Reporting ( reporting )
import Picrochole.Engine.Turn ( turn )

context :: (FilePath -> IO (Either String a))
        -> FilePath
        -> IO (Either String a)
context f fp = do
  res <- f fp
  case res of
    Left m -> return (Left ("Error in file " ++ fp ++ ": " ++ m))
    Right x -> return (Right x)

err :: IO (Either String a) -> IO (Maybe a)
err imx = do
  mx <- imx
  case mx of
    Left m -> do
      putStrLn m
      return Nothing
    Right x -> return (Just x)

(&>) :: IO (Maybe a) -> IO (Either String b) -> IO (Maybe (a, b))
(&>) imx imy = do
  mx <- imx
  my <- imy
  case my of
    Left m -> do
      putStrLn m
      return Nothing
    Right y -> case mx of
      Just x -> return (Just (x, y))
      Nothing -> return Nothing

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
  cond <- doesPathExist dir
  case cond of
    False -> putStrLn "unable to find the game directory"
    True -> do
      mx <- (err (context readAtlas (dir </> "atlas.json")))
            &> (context readInitiative (dir </> "initiative.json"))
            &> (context readUnits (dir </> "current-units.json"))
            &> (context readOrders (dir </> "orders.json"))
            &> (context readReports (dir </> "reports.json"))
            &> (context readPlan (dir </> "ia-plan.json"))
            &> (context readConfig (dir </> "config.json"))
            &> (context readCurrentTurn (dir </> "current-turn.json"))
      case mx of
        Nothing -> return ()
        Just ((((((((atlas), initiative), units), orders), reports), iaPlan), config), tcount) -> do

          let routes = route atlas initiative tcount units orders (getHQ config)
          let units' = turn atlas initiative routes units
          let tcount' = tcount + 1
          let reports' = reporting atlas initiative tcount' (getHQ config) units' reports
          let orders' = schedule atlas tcount' units' iaHQ ia lim iaPlan reports' orders

          createDirectoryIfMissing False (dir </> "past")

          writeReports (dir </> "reports.json") reports'
          writeOrders (dir </> "orders.json") orders'
          writeCurrentTurn (dir </> "current-turn.json") tcount'

          renameFile (dir </> "current-units.json") (dir </> ("units-"  ++ show tcount' ++ ".json"))
          writeUnits (dir </> "current-units.json") units'

            where

              ia = iaFaction config
              iaHQ = getHQ config ia
              lim = limit config
