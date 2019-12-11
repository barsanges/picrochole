{- |
   Module      : Picrochole.Actions.Train
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Train@ describes the effect of a supply train on the world.
-}

module Picrochole.Actions.Train (
  mkTrain,
  mkTrainName
  ) where

import Data.Sequence ( Seq(..), singleton )

import Picrochole.Actions.Action
import Picrochole.Containers.Graph
import Picrochole.Containers.World
import Picrochole.Misc.Env
import Picrochole.Misc.Time

mkTrain :: Env -> ActionID -> UTCTime -> ActionID -> Path -> Action
mkTrain env name lupdate target path = Action { apply = apply' env name lupdate target path }

apply' :: Env -> ActionID -> UTCTime -> ActionID -> Path -> World -> (Seq Action, World)
apply' env me lupdate target path w = if exist me (graph w)
                                      then (singleton x', w')
                                      else (Empty, w)
  where
    g = graph w
    g' | isClose me target g = remove me $ unload me target g
       | periodOfDay env t' == Day = moveAlongWhileClear dt me path g
       | otherwise = g
    t' = addUTCTime (durationTrainAction env) (currentTime w)
    dt = diffUTCTime t' lupdate
    w' = w { graph = g', currentTime = t' }
    path' = cutBefore (position me g') path
    x' = mkTrain env me t' target path'

mkTrainName :: ActionID -> UTCTime -> Int -> ActionID
mkTrainName storeID time i = ActionID $ show storeID ++ show time ++ show i