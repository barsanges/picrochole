{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.Data.Units
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les unités des deux camps.
-}

module Picrochole.Data.Units
  ( Units
  , Unit
  , Position(..)
  , module Picrochole.Data.Structs.XsMap
  , unitKey
  , faction
  , kind
  , strength
  , position
  , location
  , progress
  , location'
  , position'
  , locations
  , decrStrength
  , setPosition
  , removeFaction
  , getDist
  , capacityLeft
  , hasUnit
  , isContested
  , readUnits
  , readUnit
  , writeUnits
  , showUnit
  , approxEq
  ) where

import Data.Aeson ( eitherDecodeFileStrict, encodeFile )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Vector as V

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Structs.XsMap
import qualified Picrochole.JSON.Units as J
import Picrochole.JSON.Utils

-- | Une unité du plateau de jeu.
data Unit = Unit { unitKey_ :: UnitKey
                 , faction_ :: Faction
                 , kind_ :: UnitKind
                 , strength_ :: Double
                 , position_ :: Position
                 }
  deriving Show

-- | Position d'une unité sur le plateau de jeu.
data Position = Position { currentCell :: CellKey
                         , currentProgress :: Maybe Double
                         }
  deriving Show

-- | Les unités des deux camps.
type Units = XsMap CellKey UnitKey Faction Unit

-- | Indique si deux unités sont approximativement égales (i.e. : les champs
-- flottants sont comparés à une précision de 1e-12).
approxEq :: Unit -> Unit -> Bool
approxEq x y = (unitKey x == unitKey y)
               && (faction x == faction y)
               && (kind x == kind y)
               && (location x == location y)
               && (doubleComp (strength x) (strength y))
               && sameProgress
  where
    doubleComp u v = abs (u - v) < 1e-12
    sameProgress = case (progress x, progress y) of
      (Just px, Just py) -> doubleComp px py
      _ -> False

-- | Renvoie l'identifiant de l'unité.
unitKey :: Unit -> UnitKey
unitKey = unitKey_

-- | Renvoie la faction de l'unité.
faction :: Unit -> Faction
faction = faction_

-- | Renvoie l'arme de l'unité.
kind :: Unit -> UnitKind
kind = kind_

-- | Renvoie la force de l'unité.
strength :: Unit -> Double
strength = strength_

-- | Renvoie la position de l'unité.
position :: Unit -> Position
position = position_

-- | Renvoie l'identifiant de la cellule sur laquelle se trouve l'unité.
location :: Unit -> CellKey
location = currentCell . position_

-- | Renvoie l'état d'avancement de l'unité sur sa case.
progress :: Unit -> Maybe Double
progress = currentProgress . position_

-- | Renvoie l'identifiant de la cellule sur laquelle se trouve une unité du
-- plateau de jeu.
location' :: Units -> UnitKey -> Maybe CellKey
location' xs ukey = fmap location (lookupKey ukey xs)

-- | Renvoie la position d'une unité sur le plateau de jeu.
position' :: Units -> UnitKey -> Maybe Position
position' xs ukey = fmap position (lookupKey ukey xs)

-- | Renvoie l'emplacement de toutes les unités d'un camp.
locations :: Units -> Faction -> Set CellKey
locations xs f = foldr go S.empty xs
  where
    go :: Unit -> Set CellKey -> Set CellKey
    go u s = if faction u == f
             then S.insert (location u) s
             else s

-- | Diminue la force d'une unité sur le plateau de jeu.
decrStrength :: UnitKey -> Double -> Units -> Units
decrStrength uk ds xs = case lookupKey uk xs of
  Nothing -> xs
  Just u -> if s' > 0
            then insertKey uk u' xs
            else deleteKey uk xs
    where
      s' = (strength u) - ds
      u' = u { strength_ = s' }

-- | Supprime toutes les unités d'un camp sur la case donnée.
removeFaction :: Faction -> CellKey -> Units -> Units
removeFaction f ck xs = foldr go xs bunits
  where
    bunits = lookupLocationContent ck xs

    go :: Unit -> Units -> Units
    go u ys = if faction u == f
              then deleteKey (unitKey u) ys
              else ys

-- | Change la position d'une unité sur le plateau de jeu.
setPosition :: UnitKey -> Position -> Units -> Units
setPosition ukey pos xs = case lookupKey ukey xs of
  Nothing -> xs
  Just u -> insertKey ukey u' xs
    where
      u' = u { position_ = pos }

-- | Renvoie la distance à vol d'oiseau entre deux unités.
getDist :: Atlas -> Units -> UnitKey -> UnitKey -> Maybe Int
getDist atlas xs u v = do
  u' <- lookupKey u xs
  v' <- lookupKey v xs
  dist (gridSize atlas) (location u') (location v')

-- | Renvoie la capacité d'accueil restante de la case donnée.
capacityLeft :: Atlas -> Units -> Faction -> CellKey -> Double
capacityLeft atlas xs f ck = foldr go maxCapacity bunits
  where
    maxCapacity = capacity (getHex atlas ck)
    bunits = lookupLocationContent ck xs

    go :: Unit -> Double -> Double
    go u c = if faction u == f
             then c - (strength u)
             else c

-- | Indique si la case donnée contient une unité du camp indiqué.
hasUnit :: Units -> Faction -> CellKey -> Bool
hasUnit xs f ckey = any go (lookupLocationContent ckey xs)
  where
    go :: Unit -> Bool
    go u = (faction u) == f

-- | Indique si la case donnée contient des unités des deux camps.
isContested :: Units -> CellKey -> Bool
isContested xs ckey = (hasUnit xs Blue ckey) && (hasUnit xs Red ckey)

-- | Construit une instance de `Units` à partir d'un fichier JSON.
readUnits :: FilePath -> IO (Either String Units)
readUnits fp = do
  mxs <- eitherDecodeFileStrict fp
  case mxs of
    Left m -> return (Left m)
    Right xs -> do
      case parseMap readCellKey go xs of
        Left m -> return (Left m)
        Right xs' -> return (Right (fromMap location unitKey xs'))

  where

    go :: CellKey -> J.CellContent -> Either String (Either Faction [Unit])
    go _ (J.Marker txt) = fmap Left (readFaction txt)
    go ckey (J.Units vec) = case parseVector (readUnit ckey) vec of
      Left m -> Left m
      Right vec' -> Right (Right (V.toList vec'))

-- | Crée une instance de `Unit` à partir de paramètres lus dans un JSON.
readUnit :: CellKey -> J.Unit -> Either String Unit
readUnit ckey u = do
  f <- readFaction (J.faction u)
  ukind <- readUnitKind (J.kind u)
  let ukey = UK (J.unitKey u)
  return Unit { unitKey_ = ukey
              , faction_ = f
              , kind_ = ukind
              , strength_ = J.strength u
              , position_ = Position { currentCell = ckey
                                     , currentProgress = J.progress u
                                     }
              }

-- | Enregistre l'instance de `Units` dans le fichier indiqué.
writeUnits :: FilePath -> Units -> IO ()
writeUnits fp xs = encodeFile fp xs'

  where

    xs' = M.mapKeys fkey (M.map fvalue (toMap xs))

    fkey :: CellKey -> Text
    fkey (CK x) = T.pack . show $ x

    fvalue :: Either Faction [Unit] -> J.CellContent
    fvalue (Left f) = J.Marker (showFaction f)
    fvalue (Right us) = J.Units (fmap showUnit (V.fromList us))

-- | Convertit une instance de `Unit` en une structure prête à être
-- sérialisée en JSON.
showUnit :: Unit -> J.Unit
showUnit x = J.Unit { J.unitKey = rawUK (unitKey x)
                    , J.faction = showFaction (faction x)
                    , J.kind = showUnitKind (kind x)
                    , J.strength = strength x
                    , J.progress = progress x
                    }
  where
    rawUK :: UnitKey -> Text
    rawUK (UK u) = u
