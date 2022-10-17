{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.Data.Base
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Types de base pour décrire le jeu.
-}

module Picrochole.Data.Base
  ( TurnCount
  , readCurrentTurn
  , writeCurrentTurn
  , Faction(..)
  , readFaction
  , showFaction
  , opponent
  , UnitKey(..)
  , UnitKind(..)
  , readUnitKind
  , showUnitKind
  , speed
  ) where

import Data.Aeson ( eitherDecodeFileStrict, encodeFile )
import qualified Data.Text as T

import Picrochole.Data.Atlas

-- | Numéro du tour en cours.
type TurnCount = Int

-- | Lit un fichier contenant l'identifiant du tour courant.
readCurrentTurn :: FilePath -> IO (Either String TurnCount)
readCurrentTurn = eitherDecodeFileStrict

-- | Enregistre l'identifiant du tour dans le fichier indiqué.
writeCurrentTurn :: FilePath -> TurnCount -> IO ()
writeCurrentTurn = encodeFile

-- | Faction à laquelle appartient une unité.
data Faction = Blue
             | Red
  deriving (Eq, Show)

-- | Transforme une chaîne de caractères en un identifiant de faction.
readFaction :: T.Text -> Either String Faction
readFaction txt = case T.toLower (T.strip txt) of
  "blue" -> Right Blue
  "red" -> Right Red
  _ -> Left ("unable to parse " ++ (T.unpack txt) ++ " as a faction name")

-- | Transforme un identifiant de faction en une chaîne de caractères.
showFaction :: Faction -> T.Text
showFaction Blue = "blue"
showFaction Red = "red"

-- | Renvoie la faction adverse.
opponent :: Faction -> Faction
opponent Blue = Red
opponent Red = Blue

-- | Identifiant unique d'une unité.
newtype UnitKey = UK T.Text
  deriving (Eq, Ord, Show)

-- | Arme d'une unité.
data UnitKind = Infantry
              | Cavalry
              | Artillery
  deriving (Eq, Show)

-- | Transforme une chaîne de caractères en un type d'unité.
readUnitKind :: T.Text -> Either String UnitKind
readUnitKind txt = case T.toLower (T.strip txt) of
  "infantry" -> Right Infantry
  "cavalry" -> Right Cavalry
  "artillery" -> Right Artillery
  _ -> Left ("unable to parse " ++ (T.unpack txt) ++ " as a unit kind")

-- | Transforme un type d'unité en une chaîne de caractères.
showUnitKind :: UnitKind -> T.Text
showUnitKind Infantry = "infantry"
showUnitKind Cavalry = "cavalry"
showUnitKind Artillery = "artillery"

-- | Renvoie la vitesse à laquelle un type d'unité progresse sur un type de
-- case.
speed :: UnitKind -> Topography -> Double
speed Cavalry Road = 2
speed Infantry Road = 1
speed Artillery Road = 1
speed Cavalry Land = 1
speed Infantry Land = 0.5
speed Artillery Land = 0.25
speed _ Water = 0
