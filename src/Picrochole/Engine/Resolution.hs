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
import Picrochole.Engine.Decision
import Picrochole.Engine.Update

-- | Résout le tour de l'unité courante.
runResolution :: NominalDiffTime
              -> (UTCTime, Seq UnitKey, CStats, CActions)
              -> (UTCTime, Seq UnitKey, CStats, CActions)
runResolution _ (t, Empty, cs, ca) = (t, Empty, cs, ca)
runResolution dt (t, k :<| ks, cs, ca) = (t', ks |> k, cs', ca')
      where
        t' = T.addUTCTime dt t
        cs' = runUpdate t k cs
        ca' = runDecision k ca
