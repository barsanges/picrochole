{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Picrochole.IO.JSON
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON des différents objets du jeu.
-}

module Picrochole.IO.JSON
  ( readPlan
  , readOrders
  , readReports
  , writeOrders
  , writeReports
  , readAtlas
  , readInitiative
  , writeInitiative
  , readUnits
  , writeUnits
  ) where

import Data.Aeson
import qualified Data.Set as S
import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Board
import Picrochole.Data.Mail
import qualified Picrochole.Data.Mail as PM
import Picrochole.Data.Plan
import Picrochole.Data.Structs.XsMap

-- Pour mémoire :
--   toJSON :: a -> Value
--   parseJSON :: Value -> Parser a

instance ToJSON CellKey
instance ToJSONKey CellKey
instance FromJSON CellKey
instance FromJSONKey CellKey

instance ToJSON Faction
instance FromJSON Faction

instance ToJSON UnitKey
instance FromJSON UnitKey

instance ToJSON UnitKind
instance FromJSON UnitKind

instance FromJSON Objective where
  parseJSON = withObject "Objective" go
    where
      go v = do
        t <- v .: "target"
        a <- v .: "assigned"
        r <- v .: "reinforcements"
        return Objective { target = t
                         , assigned = S.fromList a
                         , reinforcements = S.fromList r
                         }

instance FromJSON Plan where
  parseJSON = withObject "Plan" go
    where
      go v = do
        s <- v .: "subordinates"
        o <- v .: "objectives"
        return Plan { subordinates = S.fromList s
                    , objectives = o
                    }

-- | Construit une instance de `Plan` à partir d'un fichier JSON.
readPlan :: FilePath -> IO (Either String Plan)
readPlan = eitherDecodeFileStrict

instance FromJSON Unit where
  parseJSON = withObject "Unit" go
    where
      go v = do
        ky <- v .: "key"
        f <- v .: "faction"
        ki <- v .: "kind"
        s <- v .: "strength"
        l <- v .: "location"
        p <- v .: "progress"
        return (mkUnit ky f ki s l p)

instance ToJSON Unit where
  toJSON x = object [ "key" .= unitKey x
                    , "faction" .= faction x
                    , "kind" .= kind x
                    , "strength" .= strength x
                    , "location" .= location x
                    , "progress" .= progress x
                    ]

instance FromJSON a => FromJSON (Msg a) where
  parseJSON = withObject "Msg" go
    where
      go v = do
        f <- v .: "from"
        t <- v .: "to"
        s <- v .: "sent"
        r <- v .: "received"
        c <- v .: "content"
        return Msg { header = Header { from = f
                                     , to = t
                                     , sent = s
                                     , received = r
                                     }
                   , content = c
                   }

instance ToJSON a => ToJSON (Msg a) where
  toJSON x = object [ "from" .= (from . header) x
                    , "to" .= (to . header) x
                    , "sent" .= (sent . header) x
                    , "received" .= (received . header) x
                    , "content" .= content x
                    ]

-- | Construit une instance de `Register Order` à partir d'un fichier JSON.
readOrders :: FilePath -> IO (Either String (Register Order))
readOrders fp = do
  mxs <- eitherDecodeFileStrict fp
  case mxs of
    Left m -> return (Left m)
    Right xs -> return (Right (PM.fromVector xs))

-- | Construit une instance de `Register Report` à partir d'un fichier JSON.
readReports :: FilePath -> IO (Either String (Register Report))
readReports fp = do
  mxs <- eitherDecodeFileStrict fp
  case mxs of
    Left m -> return (Left m)
    Right xs -> return (Right (PM.fromVector xs))

-- | Enregistre l'instance de `Register Order` dans le fichier indiqué.
writeOrders :: FilePath -> Register Order -> IO ()
writeOrders fp x = encodeFile fp (PM.toVector x)

-- | Enregistre l'instance de `Register Report` dans le fichier indiqué.
writeReports :: FilePath -> Register Report -> IO ()
writeReports fp x = encodeFile fp (PM.toVector x)

-- | Construit une instance de `[UnitKey]` à partir d'un fichier JSON.
readInitiative :: FilePath -> IO (Either String [UnitKey])
readInitiative = eitherDecodeFileStrict

-- | Enregistre l'instance de `[UnitKey]` dans le fichier indiqué.
writeInitiative :: FilePath -> [UnitKey] -> IO ()
writeInitiative = encodeFile

-- | Construit un conteneur d'unités à partir d'un fichier JSON.
readUnits :: FilePath -> IO (Either String (XsMap CellKey UnitKey Faction Unit))
readUnits fp = do
  mxs <- eitherDecodeFileStrict fp
  case mxs of
    Left m -> return (Left m)
    Right xs -> return (Right (fromMap location unitKey xs))

-- | Enregistre le conteneur d'unités dans le fichier indiqué.
writeUnits :: FilePath -> XsMap CellKey UnitKey Faction Unit -> IO ()
writeUnits fp xs = encodeFile fp (toMap xs)
