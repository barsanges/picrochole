{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Utils
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Utilitaires pour la sérialisation en JSON.
-}

module Picrochole.JSON.Utils
  ( normalize
  , parseVector
  , parseMap
  ) where

import Data.Either ( partitionEithers )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Vector ( Vector )
import qualified Data.Vector as V

-- | Normalise une chaîne de caractères en la convertissant en minuscules et
-- en supprimant les espaces inutiles au début et à la fin.
normalize :: Text -> Text
normalize = T.toLower . T.strip

-- | `fmap` sur un vecteur, avec une possibilité d'échec.
parseVector :: (a -> Either String b) -> Vector a -> Either String (Vector b)
parseVector f vec = case lefts of
  [] -> Right (V.fromList rights)
  (m:_) -> Left m

  where

    tmp = fmap f (V.toList vec)
    (lefts, rights) = partitionEithers tmp

-- | Sorte de `fmap` sur les couples clef / valeur de la map, avec une
-- possibilité d'échec.
parseMap :: (Ord k1, Ord k2)
         => (k1 -> Either String k2)
         -> (k2 -> v1 -> Either String v2)
         -> Map k1 v1
         -> Either String (Map k2 v2)
parseMap fkey fvalue xs = M.foldrWithKey go (Right M.empty) xs
  where
    go _ _ (Left m) = Left m
    go k v (Right xs') = do
      k' <- fkey k
      v' <- fvalue k' v
      return (M.insert k' v' xs')
