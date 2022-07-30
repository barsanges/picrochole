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
  ) where

import Data.Aeson
import qualified Data.Set as S
import qualified Data.Vector as V
import Picrochole.Data.Base
import Picrochole.Data.Plan
import Picrochole.Data.Utils.HexGrid

-- Pour mémoire :
--   toJSON :: a -> Value
--   parseJSON :: Value -> Parser a

instance ToJSON CellKey where
  toJSON (CK x y) = Array (V.fromList [toJSON x, toJSON y])

instance FromJSON CellKey where
  parseJSON = withArray "CellKey" go
    where
      go xs =
        if V.length xs == 2
        then do
          x <- parseJSON (xs V.! 0)
          y <- parseJSON (xs V.! 1)
          return (CK x y)
        else fail ("expected an array of length 2, got " ++ show (V.length xs) ++ " instead")

instance ToJSON Faction
instance FromJSON Faction

instance ToJSON UnitKey
instance FromJSON UnitKey

instance ToJSON UnitKind
instance FromJSON UnitKind

instance FromJSON Tile

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
