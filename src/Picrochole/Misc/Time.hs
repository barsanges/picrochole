{- |
   Module      : Picrochole.Misc.Time
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

Functions and types to handle the in-game time.
-}

module Picrochole.Misc.Time (
  UTCTime(..),
  NominalDiffTime,
  PeriodOfDay(..),
  addUTCTime,
  diffUTCTime
  ) where

import Data.Time ( UTCTime(..),
                   NominalDiffTime,
                   addUTCTime,
                   diffUTCTime,
                 )

data PeriodOfDay = Day | Night | Twilight
  deriving (Eq, Show)