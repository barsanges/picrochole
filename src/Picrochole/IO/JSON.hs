{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Picrochole.IO.JSON
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON des différents objets du jeu.
-}

module Picrochole.IO.JSON
  ( readUnits
  , writeUnits
  ) where

import Data.Aeson
import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Board
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
