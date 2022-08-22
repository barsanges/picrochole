{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Utils
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Utilitaires pour la sérialisation en JSON.
-}

module Picrochole.JSON.Utils
  ( normalize
  , eitherFmap
  ) where

import Data.Either ( partitionEithers )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Vector ( Vector )
import qualified Data.Vector as V

-- | Normalise une chaîne de caractères en la convertissant en minuscules et
-- en supprimant les espaces inutiles au début et à la fin.
normalize :: Text -> Text
normalize = T.toLower . T.strip

-- | `fmap` sur un vecteur, avec une possibilité d'échec.
eitherFmap :: (a -> Either String b) -> Vector a -> Either String (Vector b)
eitherFmap f vec = case lefts of
  [] -> Right (V.fromList rights)
  (m:_) -> Left m

  where

    tmp = fmap f (V.toList vec)
    (lefts, rights) = partitionEithers tmp
