{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.Data.Atlas
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Géographie du jeu.
-}

module Picrochole.Data.Atlas
  ( Atlas
  , CellParams(..)
  , module Picrochole.Data.Utils.HexGrid
  , readAtlas
  ) where

import Data.Aeson ( eitherDecodeFileStrict )
import Data.Either ( partitionEithers )
import qualified Data.Text as T
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Picrochole.Data.Base
import Picrochole.Data.Utils.HexGrid
import qualified Picrochole.JSON.Atlas as J

-- | Paramètres immuables d'une case du plateau de jeu.
data CellParams = CP { tile_ :: Tile
                     , capacity_ :: Double
                     } -- FIXME : inscrire dans les types la distinction crossable / uncrossable
  deriving Show

-- | Géographie du jeu.
type Atlas = HexGrid CellParams

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

    go :: Vector J.CellParams -> Either String (Vector CellParams)
    go vec = case lefts of
      [] -> Right (V.fromList rights)
      (m:_) -> Left m

      where

        tmp = fmap readCP (V.toList vec)
        (lefts, rights) = partitionEithers tmp

-- | Crée une instance de `CellParams` à partir de paramètres lus dans un JSON.
readCP :: J.CellParams -> Either String CellParams
readCP cp =
  if J.capacity cp >= 0
  then case J.tile cp of
         "road" -> Right (CP { tile_ = Road
                             , capacity_ = J.capacity cp
                             })
         "land" -> Right (CP { tile_ = Land
                             , capacity_ = J.capacity cp
                             })
         "water" -> Right (CP { tile_ = Water
                              , capacity_ = 0
                              })
         _ -> Left ("unable to parse " ++ (T.unpack $ J.tile cp) ++ "as a tile")
  else Left "a tile should have a positive capacity"
