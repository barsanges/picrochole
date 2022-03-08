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

-- | Laisse en place l'unité courante.
stay :: UTCTime -> Stats -> Stats
stay t' s = s { uLastUpdate = t' } -- FIXME : mutualiser cette opération.

-- | Déplace l'unité courante.
move :: UTCTime
     -> Double
     -> [LocationKey]
     -> Stats
     -> CStats
     -> Stats
move t' done fullPath s cs = s { uLastUpdate = t'
                               , uLocation = loc
                               , uAction = action
                               }
  where
    f = uFaction s
    speed = uSpeed s
    (loc, action) = go (timeSinceLastUpdate t' s) done fullPath (uLocation s)

    go :: NominalDiffTime -> Double -> [LocationKey] -> LocationKey -> (LocationKey, Action)
    go dt x path current = if x' > 1
                           then case path of
                                  [] -> (current, Still)
                                  (p:ps) -> if null (findLocationOthers p f cs)
                                            then go dt' 0 ps p
                                            else (current, Still)
                           else (current, Moving x' path)
      where
        x' = x + (realToFrac dt) * speed
        dt' = dt - (realToFrac ((1 - x) / speed))

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
        s' = case uAction s of
          Still -> stay t' s
          Moving done path -> move t' done path s cs
        cs' = insertCStats s' cs
