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

-- | Résout le tour de l'unité courante.
runResolution :: NominalDiffTime
              -> (UTCTime, Seq UnitKey)
              -> (UTCTime, Seq UnitKey)
runResolution _ (t, Empty) = (t, Empty)
runResolution dt (t, k :<| ks) = (t', ks |> k)
  where
    t' = T.addUTCTime dt t
