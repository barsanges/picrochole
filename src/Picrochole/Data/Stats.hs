{- |
   Module      : Picrochole.Data.Stats
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour l'état des unités.
-}

module Picrochole.Data.Stats
  ( Stats(..)
  , CStats(..)
  , lookupCStats
  , insertCStats
  ) where

import Data.Time ( UTCTime )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M -- FIXME : IntMap ?
import Picrochole.Data.Action
import Picrochole.Data.Keys

-- | Caractéristiques d'une unité.
data Stats = Stats { uLocation :: NE.NonEmpty LocationKey
                   , uAction :: Action
                   , uLastUpdate :: UTCTime
                   }
  deriving Show

-- | L'ensemble des unités de la partie.
newtype CStats = CS (M.Map UnitKey Stats)

-- | Recherche une unité dans le conteneur des unités.
lookupCStats :: UnitKey -> CStats -> Maybe Stats
lookupCStats k (CS m) = M.lookup k m

-- | Met à jour le conteneur des unités.
insertCStats :: UnitKey -> Stats -> CStats -> CStats
insertCStats k s (CS m) = CS (M.insert k s m)
