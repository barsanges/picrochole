{- |
   Module      : Picrochole.Data.Config
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Paramètres généraux de la partie.
-}

module Picrochole.Data.Config
  ( Config(..)
  , readConfig
  , getHQ
  ) where

import Data.Aeson ( eitherDecodeFileStrict )

import Picrochole.Data.Base
import qualified Picrochole.JSON.Config as J

-- | Paramètres généraux de la partie.
data Config = Config { iaFaction :: Faction
                     , hqBlue :: UnitKey
                     , hqRed :: UnitKey
                     , limit :: Int
                     }

-- | Renvoie l'état-major de la faction indiquée.
getHQ :: Config -> Faction -> UnitKey
getHQ config Blue = hqBlue config
getHQ config Red = hqRed config

-- | Lit un fichier contenant les paramètres généraux de la partie.
readConfig :: FilePath -> IO (Either String Config)
readConfig fp = do
  mcfg <- eitherDecodeFileStrict fp
  case mcfg of
    Left m -> return (Left m)
    Right cfg -> case readFaction (J.iaFaction cfg) of
      Right f -> return (Right Config { iaFaction = f
                                      , hqBlue = UK (J.hqBlue cfg)
                                      , hqRed = UK (J.hqRed cfg)
                                      , limit = J.limit cfg
                                      })
      Left m -> return (Left m)
