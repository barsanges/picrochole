{- |
   Module      : Main
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Envoie un ordre à un subordonné dans une partie de Picrochole.
-}

module Main where

import qualified Data.Text as T
import Options.Applicative ( ParserInfo, (<**>) )
import qualified Options.Applicative as O
import System.FilePath.Posix ( (</>) )

import Picrochole.Data.Base
import Picrochole.Data.Config
import Picrochole.Data.Orders
import Picrochole.Data.Units
import Picrochole.Data.Structs.HexGrid

import Picrochole.JSON.Pieces

-- | Arguments de la ligne de commande.
data Args = Args { dir :: FilePath
                 , addressee :: String
                 , destination :: Int
                 }

-- | Ligne de commande pour Picrochole.
args :: ParserInfo Args
args = O.info ( argsParser <**> O.helper ) desc
  where
    argsParser = Args
      <$> ( O.strArgument
            ( O.metavar "DIR"
              <> O.help "Directory containing the current game" ))
      <*> ( O.strOption
            ( O.long "addressee"
              <> O.metavar "A"
              <> O.help "The ID of the subordinate to which the order is sent" ))
      <*> ( O.option O.auto
            ( O.long "destination"
              <> O.metavar "D"
              <> O.help "The ID (an integer) of the destination that the \
                        \subordinate should reach" ) )
    desc = O.fullDesc
           <> O.header "picrochole - Game engine for pre-napoleonic and napoleonic warfare"
           <> O.progDesc "Send an order (i.e. a destination to reach) to a subordinate"

-- | Résout le tour courant.
main :: IO ()
main = do
  cli <- O.execParser args
  meverything <- loadEverythingDir (dir cli)
  case meverything of
    Left m -> putStrLn m
    Right everything -> do
      let atlas = eatlas everything
      let config = econfig everything
      let tcount = ecurrentTurn everything
      let orders = eorders everything
      let units = eunits everything

      let ia = iaFaction config
      let playerHQ = getHQ config (opponent ia)

      let ukey = UK (T.pack (addressee cli))
      let objective = CK (destination cli)

      if isInsideGrid atlas objective
        then do
        let d = getDist atlas units ukey playerHQ
        let h = mkHeader tcount playerHQ ukey d
        let orders' = send h objective orders
        writeOrders ((dir cli) </> "orders.json") orders'

        else putStrLn $ "the given destination (" ++ show (destination cli)
             ++ ") does not exist"
