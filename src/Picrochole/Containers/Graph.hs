{- |
   Module      : Picrochole.Containers.Graph
   Copyright   : Copyright (C) 2019 barsanges
   License     : GNU GPL, version 3

@Graph@ contains the map of the world and the objects populating it.
-}

module Picrochole.Containers.Graph (
  Graph,
  Position,
  Path,
  exist,
  isClose,
  isClear,
  owner,
  position,
  unload,
  remove,
  moveAlong,
  moveAlongWhileClear,
  mkPath,
  cutBefore,
  consume,
  rest,
  weary,
  fight,
  shouldRout,
  shouldRally,
  isRouted,
  prevPosition,
  isGoingBackward,
  rout,
  rally,
  ) where

import Picrochole.Actions.ActionID
import Picrochole.Misc.Faction
import Picrochole.Misc.Position
import Picrochole.Misc.Time

data Graph = Graph

exist :: ActionID -> Graph -> Bool
exist = undefined -- FIXME

isClose :: ActionID -> ActionID -> Graph -> Bool
isClose = undefined -- FIXME

isClear :: Faction -> Graph -> Bool
isClear = undefined -- FIXME

owner :: ActionID -> Graph -> Faction
owner = undefined

position :: ActionID -> Graph -> Position
position = undefined -- FIXME

unload :: ActionID ->  ActionID -> Graph -> Graph
unload = undefined -- FIXME

remove :: ActionID -> Graph -> Graph
remove = undefined -- FIXME

-- TODO : stop as soon as one encounters an enemy.
moveAlong :: NominalDiffTime -> ActionID -> Path -> Graph -> Graph
moveAlong = undefined -- FIXME

moveAlongWhileClear :: NominalDiffTime -> ActionID -> Path -> Graph -> Graph
moveAlongWhileClear = undefined -- FIXME

mkPath :: Position -> Position -> Graph -> Path
mkPath = undefined -- FIXME

cutBefore :: Position -> Path -> Path
cutBefore = undefined -- FIXME

consume :: NominalDiffTime -> ActionID -> Graph -> Graph
consume = undefined -- FIXME

rest :: NominalDiffTime -> ActionID -> Graph -> Graph
rest = undefined -- FIXME

weary :: NominalDiffTime -> ActionID -> Graph -> Graph
weary = undefined -- FIXME

fight :: NominalDiffTime -> ActionID -> Graph -> Graph
fight = undefined -- FIXME

shouldRout :: ActionID -> Graph -> Bool
shouldRout = undefined -- FIXME

shouldRally :: ActionID -> Graph -> Bool
shouldRally = undefined -- FIXME

isRouted :: ActionID -> Graph -> Bool
isRouted = undefined -- FIXME

prevPosition :: ActionID -> Graph -> Position
prevPosition = undefined -- FIXME

isGoingBackward :: ActionID -> Path -> Graph -> Bool
isGoingBackward = undefined -- FIXME

rout :: ActionID -> Graph -> Graph
rout = undefined -- FIXME

rally :: ActionID -> Graph -> Graph
rally = undefined -- FIXME