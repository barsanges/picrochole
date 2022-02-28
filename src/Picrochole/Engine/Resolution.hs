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
import Picrochole.Data.Stats

-- | Résout le tour de l'unité courante.
runResolution :: NominalDiffTime
              -> (UTCTime, Seq UnitKey, CStats)
              -> (UTCTime, Seq UnitKey, CStats)
runResolution _ (t, Empty, cs) = (t, Empty, cs)
runResolution dt (t, k :<| ks, cs) =
  case lookupCStats k cs of
    Nothing -> (t, ks, cs)
    Just s -> (t', ks |> k, cs')
      where
        t' = T.addUTCTime dt t
        s' = s { uLastUpdate = t' }
        cs' = insertCStats k s' cs
