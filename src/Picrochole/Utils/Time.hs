{- |
   Module      : Picrochole.Utils.Time
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

TODO.
-}

module Picrochole.Utils.Time
  ( fromSeconds
  , toSeconds
  ) where

import Data.Time ( NominalDiffTime )
import qualified Data.Time as T

-- | Exprime un nombre de secondes comme un 'NominalDiffTime'.
fromSeconds :: Double -> NominalDiffTime
fromSeconds x = T.secondsToNominalDiffTime (realToFrac x)

-- | Exprime un 'NominalDiffTime' comme un nombre de secondes.
toSeconds :: NominalDiffTime -> Double
toSeconds dt = realToFrac (T.nominalDiffTimeToSeconds dt)
