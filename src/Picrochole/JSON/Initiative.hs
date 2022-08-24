{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Picrochole.JSON.Initiative
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON de l'ordre d'initiative des unités.
-}

module Picrochole.JSON.Initiative
  ( Initiative
  ) where

import Data.Text ( Text )
import Data.Vector ( Vector )

-- | Ordre d'initiative des unités.
type Initiative = Vector Text
