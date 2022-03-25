{- |
   Module      : Picrochole.Data.Action
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Actions entreprises par les unités.
-}

module Picrochole.Data.Action
  ( Action(..)
  , CActions
  , insertCActions
  , lookupCActions
  ) where

import qualified Data.Map as M -- FIXME : IntMap ?
import Picrochole.Data.Keys

-- | Action entreprise par une unité.
data Action = Still
            | Moving Double LocationKey [LocationKey]
  deriving (Show, Eq)
-- FIXME : introduire un type spécifique pour les chemins, qui permettent de
-- s'assurer que les cases sont contigües ?

-- | L'action associée à chaque unité.
newtype CActions = CA (M.Map UnitKey Action)
  deriving Show

-- | Met à jour le conteneur des actions.
insertCActions :: UnitKey -> Action -> CActions -> CActions
insertCActions k a (CA m) = CA (M.insert k a m)
-- FIXME : uniformiser l'interface avec des typeclasses à la MTL ?

-- | Renvoie l'action associée à une unité.
lookupCActions :: UnitKey -> CActions -> Maybe Action
lookupCActions k (CA m) = M.lookup k m
