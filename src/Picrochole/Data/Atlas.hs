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
  , topography'
  ) where

import Data.Aeson ( eitherDecodeFileStrict )
import Data.Either ( partitionEithers )
import qualified Data.Text as T
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Picrochole.Data.Structs.HexGrid
import qualified Picrochole.JSON.Atlas as J

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
      case go (J.content a) of
        Left m -> return (Left m)
        Right vec -> case fromVector gsize vec of
          Nothing -> return (Left "unable to parse the atlas, due to a mismatch between the grid size and the length of the vector")
          Just atlas -> return (Right atlas)

  where

    go :: Vector J.Tile -> Either String (Vector Tile)
    go vec = case lefts of
      [] -> Right (V.fromList rights)
      (m:_) -> Left m

      where

        tmp = fmap readTile (V.toList vec)
        (lefts, rights) = partitionEithers tmp

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
