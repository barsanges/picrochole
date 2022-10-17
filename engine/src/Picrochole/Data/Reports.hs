{- |
   Module      : Picrochole.Data.Reports
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les rapports que les unités s'échangent.
-}

module Picrochole.Data.Reports
  ( Report
  , module Picrochole.Data.Structs.Register
  , readReports
  , writeReports
  ) where

import Data.Aeson ( eitherDecodeFileStrict, encodeFile )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Vector as V

import Picrochole.Data.Atlas ( CellKey(..), readCellKey )
import Picrochole.Data.Base
import Picrochole.Data.Cell
import qualified Picrochole.Data.Structs.Bag as B
import Picrochole.Data.Structs.Register
import Picrochole.Data.Units
import qualified Picrochole.JSON.Reports as J
import Picrochole.JSON.Utils

-- | Rapport d'une unité à son état-major.
type Report = Map CellKey CellContent

-- | Construit une instance de `Register Report` à partir d'un fichier JSON.
readReports :: FilePath -> IO (Either String (Register Report))
readReports fp = do
  mxs <- eitherDecodeFileStrict fp
  case mxs of
    Left m -> return (Left m)
    Right xs -> do
      case parseVector readReport xs of
        Left m -> return (Left m)
        Right vec -> return (Right (fromVector vec))

-- | Crée une instance de `Msg Report` à partir de paramètres lus dans un JSON.
readReport :: J.Report -> Either String (Msg Report)
readReport r = if err (J.received r)
               then Left ("got a malformed message which has been received before being sent\n" ++ show r)
               else case readContent (J.content r) of
                      Left m -> Left m
                      Right c -> Right (Msg { header = Header { from = UK (J.from r)
                                                              , to = UK (J.to r)
                                                              , sent = J.sent r
                                                              , received = J.received r
                                                              }
                                            , content = c
                                            })
  where
    err :: Maybe Int -> Bool
    err Nothing = False
    err (Just x) = x < J.sent r

-- | Crée une instance de `Report` à partir de paramètres lus dans un JSON.
readContent :: Map Text J.CellContent -> Either String Report
readContent xs = parseMap readCellKey fvalue xs
  where

    fvalue :: CellKey -> J.CellContent -> Either String CellContent
    fvalue _ (J.Marker txt) = fmap Left (readFaction txt)
    fvalue ckey (J.Units vec) = case parseVector (readUnit ckey) vec of
      Left m -> Left m
      Right vec' -> Right (Right (partition (\ x -> faction x == Blue) (B.fromList . V.toList $ vec')))

-- | Enregistre l'instance de `Register Report` dans le fichier indiqué.
writeReports :: FilePath -> Register Report -> IO ()
writeReports fp x = encodeFile fp (fmap showReport (toVector x))

-- | Convertit une instance de `Msg Report` en une structure prête à être
-- sérialisée en JSON.
showReport :: Msg Report -> J.Report
showReport r = J.Report { J.from = rawUK (from . header $ r)
                        , J.to = rawUK (to . header $ r)
                        , J.sent = sent . header $ r
                        , J.received = received . header $ r
                        , J.content = M.mapKeys fkey (M.map fvalue (content r))
                        }

  where

    rawUK :: UnitKey -> Text
    rawUK (UK x) = x

    fkey :: CellKey -> Text
    fkey (CK x) = T.pack . show $ x

    fvalue :: CellContent -> J.CellContent
    fvalue (Left f) = J.Marker (showFaction f)
    fvalue (Right (xs, ys)) = J.Units (fmap showUnit (V.concat [ (V.fromList . B.toList) xs
                                                               , (V.fromList . B.toList) ys
                                                               ]))
