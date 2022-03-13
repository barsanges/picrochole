{- |
   Module      : Picrochole.Engine.Decision
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Met à jour l'action de l'unité courante.
-}

module Picrochole.Engine.Decision
  ( runDecision
  ) where

import Picrochole.Data.Action
import Picrochole.Data.Keys

-- | Met à jour l'action de l'unité courante.
runDecision :: UnitKey
            -> CActions
            -> CActions
runDecision k ca = insertCActions k Still ca -- FIXME
