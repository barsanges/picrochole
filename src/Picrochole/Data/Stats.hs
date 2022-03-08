{- |
   Module      : Picrochole.Data.Stats
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Structures pour l'état des unités.
-}

module Picrochole.Data.Stats
  ( Stats(..)
  , CStats
  , lookupCStats
  , insertCStats
  , findLocation
  , findLocationAllies
  , findLocationOthers
  , timeSinceLastUpdate
  ) where

import Data.IxSet ( Indexable(..), IxSet )
import qualified Data.IxSet as IX
import Data.Time ( UTCTime, NominalDiffTime )
import qualified Data.Time as T
import Picrochole.Data.Action
import Picrochole.Data.Keys

-- | Caractéristiques d'une unité.
data Stats = Stats { uKey :: UnitKey
                   , uLocation :: LocationKey
                   , uAction :: Action
                   , uLastUpdate :: UTCTime
                   , uSpeed :: Double
                   , uFaction :: FactionKey
                   }
  deriving (Show, Eq, Ord)
-- FIXME : sortir l'action de cette structure ? Idem pour la localisation ?
-- L'action pose problème car elle évolue durant la mise à jour des
-- statistiques, et pas uniquement dans la phase de prise de décision !
-- FIXME : faire dépendre la vitesse du type de case ?

instance Indexable Stats where
  empty = IX.ixSet
    [ IX.ixFun $ \x -> [ uKey x ]
    , IX.ixFun $ \x -> [ uLocation x ]
    , IX.ixFun $ \x -> [ uFaction x ]
    ]

-- | L'ensemble des unités de la partie.
type CStats = IxSet Stats

-- | Recherche une unité dans le conteneur des unités.
lookupCStats :: UnitKey -> CStats -> Maybe Stats
lookupCStats k ix = IX.getOne (IX.getEQ k ix)

-- | Met à jour le conteneur des unités.
insertCStats :: Stats -> CStats -> CStats
insertCStats s ix = IX.updateIx (uKey s) s ix

-- | Renvoie toutes les unités présentes à un emplacement.
findLocation :: LocationKey -> CStats -> [Stats]
findLocation k ix = IX.toList (IX.getEQ k ix) -- FIXME : renvoyer autre chose qu'une liste ?

-- | Renvoie toutes les unités présentes à un emplacement appartenant à
-- la faction donnée.
findLocationAllies :: LocationKey -> FactionKey -> CStats -> [Stats]
findLocationAllies k f ix = IX.toList (IX.getEQ f $ IX.getEQ k ix) -- FIXME : renvoyer autre chose qu'une liste ?

-- | Renvoie toutes les unités présentes à un emplacement n'appartenant pas à
-- la faction donnée.
findLocationOthers :: LocationKey -> FactionKey -> CStats -> [Stats]
findLocationOthers k f ix = filter (\ x -> uFaction x /= f) (findLocation k ix)

-- | Calcule le temps écoulé depuis la dernière mise à jour de l'unité.
timeSinceLastUpdate :: UTCTime -> Stats -> NominalDiffTime
timeSinceLastUpdate t' s = T.diffUTCTime t' (uLastUpdate s)
