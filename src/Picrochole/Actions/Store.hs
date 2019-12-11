{- |
   Module      : Picrochole.Actions.Store
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Store@ describes the effect of a supply store on the world.
-}

module Picrochole.Actions.Store (
  mkStore
  ) where

import Data.List ( foldl' )
import Data.Sequence ( Seq(..), (|>), empty )

import Picrochole.Actions.Action
import Picrochole.Actions.Train
import Picrochole.Containers.Post
import Picrochole.Containers.World
import Picrochole.Misc.Env
import Picrochole.Misc.Time

mkStore :: Env -> ActionID -> UTCTime -> Action
mkStore env name lupdate = Action { apply = apply' env name lupdate }

apply' :: Env -> ActionID -> UTCTime -> World -> (Seq Action, World)
apply' env me lupdate w = if exist me (graph w)
                          then (new |> x', w')
                          else (Empty, w)
  where
    orders = troopsStoreSince me lupdate (post w)
    -- Messages are processed by order of arrival:
    (_, new, g') = foldl' popTrain (0, empty, graph w) orders
    t' = addUTCTime (durationStoreAction env) (currentTime w)
    w' = w { graph = g', currentTime = t' }
    x' = mkStore env me t'

    popTrain :: (Int, Seq Action, Graph)
             -> Troops_Store
             -> (Int, Seq Action, Graph)
    popTrain (i, as, g0) m = if owner me g0 == senderFaction m
                             then (i+1, as |> a, g1)
                             else (i, as, g0)
      where
        name = mkTrainName me (currentTime w) i
        g1 = createTrain me name (qtyNeeded m) g0
        p = mkPath (position me g1) (senderPosition m) g1
        a = mkTrain env name t' (sender m) p