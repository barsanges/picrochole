{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.Data.Atlas
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Géographie du jeu.
-}

module Picrochole.Data.Atlas
  ( Atlas
  , Tile
  , Topography(..)
  , topography
  , capacity
  , module Picrochole.Data.Structs.HexGrid
  , readAtlas
  , readCellKey
  , topography'
  , getDiskKeys
  , approxEq
  ) where

import Data.Aeson ( eitherDecodeFileStrict )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.Read ( decimal )

import Picrochole.Data.Structs.HexGrid
import qualified Picrochole.JSON.Atlas as J
import Picrochole.JSON.Utils

-- | Paramètres immuables d'une case du plateau de jeu.
data Tile = TRoad Double
          | TLand Double
          | TWater
  deriving Show

-- | Nature du terrain sur une case.
data Topography = Road
                | Land
                | Water
  deriving (Eq, Show)

-- | Renvoie la topographie associée à une case.
topography :: Tile -> Topography
topography (TRoad _) = Road
topography (TLand _) = Land
topography TWater = Water

-- | Renvoie la capacité d'une case.
capacity :: Tile -> Double
capacity (TRoad x) = x
capacity (TLand x) = x
capacity TWater = 0

-- | Indique si deux cases sont approximativement égales (i.e. : les capacités
-- sont comparées à une précision de 1e-12).
approxEq :: Tile -> Tile -> Bool
approxEq x y = (topography x == topography y)
               && (doubleComp (capacity x) (capacity y))
  where
    doubleComp u v = abs (u - v) < 1e-12

-- | Géographie du jeu.
type Atlas = HexGrid Tile

-- | Lit un fichier contenant la géographie du jeu.
readAtlas :: FilePath -> IO (Either String Atlas)
readAtlas fp = do
  ma <- eitherDecodeFileStrict fp
  case ma of
    Left m -> return (Left m)
    Right a -> do
      let gsize = GridSize { ncols = J.ncols a
                           , nrows = J.nrows a
                           }
      case parseVector readTile (J.content a) of
        Left m -> return (Left m)
        Right vec -> case fromVector gsize vec of
          Nothing -> return (Left "unable to parse the atlas, due to a mismatch between the grid size and the length of the vector")
          Just atlas -> return (Right atlas)

-- | Crée une instance de `CellKey` à partir d'une chaîne de caractères lue dans
-- un JSON.
readCellKey :: Text -> Either String CellKey
readCellKey txt = case decimal (T.strip txt) of
  Left m -> Left m
  Right (i, txt') -> if T.null txt'
                     then Right (CK i)
                     else Left ("some characters could be parsed as a cell key" ++ (T.unpack txt'))

-- | Crée une instance de `Tile` à partir de paramètres lus dans un JSON.
readTile :: J.Tile -> Either String Tile
readTile cp =
  if J.capacity cp >= 0
  then case J.topography cp of
         "road" -> Right (TRoad (J.capacity cp))
         "land" -> Right (TLand (J.capacity cp))
         "water" -> Right TWater
         _ -> Left ("unable to parse " ++ (T.unpack $ J.topography cp) ++ "as a tile")
  else Left "a tile should have a positive capacity"

-- | Renvoie la nature du terrain sur la case donnée.
topography' :: Atlas -> CellKey -> Topography
topography' atlas ck = topography (getHex atlas ck)

-- | Renvoie les identifiants d'un disque de cases du plateau de jeu, dont le
-- centre est la case indiquée.
getDiskKeys :: Atlas -> CellKey -> Int -> [CellKey]
getDiskKeys atlas ck radius = diskKeys (gridSize atlas) ck radius
