{- |
   Module      : Picrochole.Misc.Env
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Env@ are parameters used for a whole game but whose values are unknown at
compile time.
-}

module Picrochole.Misc.Env (
  Env(..)
  ) where

import Picrochole.Misc.Time

-- FIXME : use a Reader monad.
data Env = Env
  {
    periodOfDay :: UTCTime -> PeriodOfDay,
    durationCommanderAction :: NominalDiffTime,
    durationGHQAction :: NominalDiffTime,
    durationStoreAction :: NominalDiffTime,
    durationTrainAction :: NominalDiffTime,
    durationTroopsAction :: NominalDiffTime
  }