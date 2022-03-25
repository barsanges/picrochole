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
import Picrochole.Data.Keys
import Picrochole.Data.World
import Picrochole.Engine.Decision
import Picrochole.Engine.Movement
import Picrochole.Engine.Update

-- | Etat du jeu.
data State = State { now :: UTCTime
                   , order :: Seq UnitKey
                   , world :: World
                   }
  deriving Show

-- | Résout le tour de l'unité courante.
runResolution :: NominalDiffTime
              -> State
              -> State
runResolution dt s = case order s of
  Empty -> s
  k :<| ks -> s { now = t'
                , order = ks |> k
                , world = w'
                }
    where
      t' = T.addUTCTime dt (now s)
      w = world s
      ca = cActions w
      w0 = runUpdate t' k (runMovement t' k w)
      w' = w0 { cActions = runDecision k ca }
-- FIXME : gérer la situation où l'unité n'existe plus.
