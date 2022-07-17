{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Picrochole.IO.JSON
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Sérialisation en JSON des différents objets du jeu.
-}

module Picrochole.IO.JSON
  (
  ) where

import Data.Aeson
import Data.Scientific ( toBoundedInteger )
import qualified Data.Set as S
import qualified Data.Vector as V
import Picrochole.Data.Base
import Picrochole.Data.Board
import Picrochole.Data.Mail
import Picrochole.Data.Plan
import Picrochole.Data.Utils.HexGrid

-- Pour mémoire :
--   toJSON :: a -> Value
--   parseJSON :: Value -> Parser a

instance ToJSON CellKey where
  toJSON (CK x y) = Array (V.fromList [toJSON x, toJSON y])

instance FromJSON CellKey where
  parseJSON = withArray "CellKey" go
    where
      go xs =
        if V.length xs == 2
        then do
          x <- parseJSON (xs V.! 0)
          y <- parseJSON (xs V.! 1)
          return (CK x y)
        else fail ("expected an array of length 2, got " ++ show (V.length xs) ++ " instead")

instance ToJSON Faction
instance FromJSON Faction

instance ToJSON UnitKey where
  toJSON (UK x) = toJSON x

instance FromJSON UnitKey where
  parseJSON = withScientific "UnitKey" go
    where
      go v = case toBoundedInteger v of
        Just x -> return (UK x)
        Nothing -> fail ("unable to convert " ++ show v ++ " to an integer")

instance ToJSON UnitKind
instance FromJSON UnitKind

instance FromJSON Tile
instance ToJSON Tile

instance ToJSON Unit where
  toJSON unit = object [ "key" .= unitKey unit
                       , "faction" .= faction unit
                       , "kind" .= kind unit
                       , "strength" .= strength unit
                       ]

instance FromJSON Unit where
  parseJSON = withObject "Unit" go
    where
      go v = do
        ke <- v .: "key"
        f <- v .: "faction"
        ki <- v .: "kind"
        s <- v .: "strength"
        return (mkUnit ke f ki s)

instance ToJSON CellContent where
  toJSON cContent = case cContent of
    Marker f -> object [ "marker" .= f ]
    Units b r -> object [ "blues" .= b
                        , "reds" .= r
                        ]

instance FromJSON CellContent where
  parseJSON = withObject "CellContent" go
    where
      go v = do
        mf <- v .:? "marker"
        mb <- v .:? "blues"
        mr <- v .:? "reds"
        case (mf, mb, mr) of
          (Just f, Nothing, Nothing) -> return (Marker f)
          (Just _, _, _) -> fail "must have either a marker ('marker'), or two lists of units ('blues' and 'reds'), but not both"
          (Nothing, Just b, Just r) -> return (Units b r)
          (Nothing, Just _, Nothing) -> fail "'reds' field missing"
          (Nothing, Nothing, Just _) -> fail "'blues' field missing"
          (Nothing, Nothing, Nothing) -> fail "must have either a marker ('marker'), or two lists of units ('blues' and 'reds')"

instance ToJSON ThreatLevel
instance FromJSON ThreatLevel

instance ToJSON Objective where
  toJSON obj = object [ "target" .= target obj
                      , "assigned" .= S.toList (assigned obj)
                      , "reinforcements" .= S.toList (reinforcements obj)
                      ]

instance FromJSON Objective where
  parseJSON = withObject "Objective" go
    where
      go v = do
        t <- v .: "target"
        a <- v .: "assigned"
        r <- v .: "reinforcements"
        return Objective { target = t
                         , assigned = S.fromList a
                         , reinforcements = S.fromList r
                         }

instance ToJSON Plan where
  toJSON plan = object [ "subordinates" .= S.toList (subordinates plan)
                       , "objectives" .= objectives plan
                       ]

instance FromJSON Plan where
  parseJSON = withObject "Plan" go
    where
      go v = do
        s <- v .: "subordinates"
        o <- v .: "objectives"
        return Plan { subordinates = S.fromList s
                    , objectives = o
                    }

instance ToJSON Header where
  toJSON header = object [ "from" .= from header
                         , "to" .= to header
                         , "sent" .= sent header
                         , "received" .= received header
                         ]

instance FromJSON Header where
  parseJSON = withObject "Header" go
    where
      go v = do
        f <- v .: "from"
        t <- v .: "to"
        s <- v .: "sent"
        r <- v .: "received"
        return Header { from = f
                      , to = t
                      , sent = s
                      , received = r
                      }

instance ToJSON a => ToJSON (Register a) where
  toJSON reg = Array (fmap go (toVector reg))
    where
      -- go :: (Header, a) -> Value
      go (header, x) = object [ "header" .= header
                              , "content" .= x
                              ]

instance FromJSON a => FromJSON (Register a) where
  parseJSON = withArray "Register" f
    where
      -- f :: Array -> Parser (Register a)
      f arr = fmap organize (g arr)

      -- g :: Array -> Parser (Vector (Header, a))
      g = mapM (withObject "Message" h)

      -- h :: Object -> Parser (Header, a)
      h v = do
        header <- v .: "header"
        x <- v .: "content"
        return (header, x)

instance ToJSON Post where
  toJSON post = object [ "reports" .= reports post
                       , "orders" .= orders post
                       ]

instance FromJSON Post where
  parseJSON = withObject "Post" go
    where
      go v = do
        r <- v .: "reports"
        o <- v .: "orders"
        return Post { reports = r
                    , orders = o
                    }
