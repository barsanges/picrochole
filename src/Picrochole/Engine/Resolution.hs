{- |
   Module      : Picrochole.Engine.Resolution
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Résolution du tour de l'unité courante.
-}

module Picrochole.Engine.Resolution
  ( runResolution
  ) where

import Data.Time ( UTCTime, NominalDiffTime )
import qualified Data.Time as T
import Data.Sequence ( Seq(..), (|>) )
import Picrochole.Data.Action
import Picrochole.Data.Keys
import Picrochole.Data.Stats
import Picrochole.Data.Store
import Picrochole.Engine.Decision
import Picrochole.Engine.Logistics
import Picrochole.Engine.Update

-- | Ordre de résolution.
type Order = Seq (Either StoreKey UnitKey)

-- | Etat du jeu.
data State = State { now :: UTCTime
                   , order :: Order
                   , cActions :: CActions
                   , cStats :: CStats
                   , cStores :: CStores
                   }
  deriving Show

-- | Résout le tour de l'unité courante.
runResolution :: NominalDiffTime
              -> State
              -> State
runResolution dt s = case order s of
  Empty -> s
  (Left k) :<| ks -> s { now = t'
                       , order = ks |> (Left k)
                       , cStores = runLogistics k ct
                       }
  (Right k) :<| ks -> s { now = t'
                        , order = ks |> (Right k)
                        , cActions = runDecision k ca
                        , cStats = runUpdate t' k cs
                        }
  where
     t' = T.addUTCTime dt (now s)
     ca = cActions s
     cs = cStats s
     ct = cStores s
