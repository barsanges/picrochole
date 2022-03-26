{- |
   Module      : Picrochole.Engine.Movement
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Met à jour la position de l'unité courante.
-}

module Picrochole.Engine.Movement
  ( runMovement
  ) where

import Data.Time ( UTCTime, NominalDiffTime )
import qualified Data.Time as T
import Picrochole.Data.Action
import Picrochole.Data.Keys
import Picrochole.Data.Stats
import Picrochole.Data.World
import Picrochole.Utils.Time
import qualified Picrochole.Utils.XsMap as Xs

-- | Met à jour la position de l'unité courante.
runMovement :: UTCTime
            -> UnitKey
            -> World
            -> World
runMovement t' k w = case Xs.lookupKey k cs of
  Nothing -> w
  Just s -> case lookupCActions k ca of
    Nothing -> error ("no action associated to the unit " ++ show k)
    Just a -> case a of
      Moving x current path -> w { cActions = ca'
                                 , cStats = cs'
                                 }
        where
          dt = T.diffUTCTime t' (uLastUpdate s)
          (lk', a') = move cs (uSpeed s) (uFaction s) dt x current path
          ca' = insertCActions k a' ca
          s' = s { uLocation = lk' }
          cs' = Xs.insertKey k s' cs
      _ -> w
  where
    ca = cActions w
    cs = cStats w

-- | Déplace une unité.
move :: CStats
     -> Double
     -> FactionKey
     -> NominalDiffTime
     -> Double
     -> LocationKey
     -> [LocationKey]
     -> (LocationKey, Action)
move cs speed f dt x current path =
  if x' > 1
  then case path of
         [] -> (current, Still)
         (p:ps) -> if hasEnnemies p f cs
                   then move cs speed f dt' 0 p []
                   else move cs speed f dt' 0 p ps
  else (current, Moving x' current path)
  where
    x' = x + (toSeconds dt) * speed
    dt' = dt - (fromSeconds ((1 - x) / speed))
