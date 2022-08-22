{- |
   Module      : Picrochole.Data.Orders
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les orders que les unités s'échangent.
-}

module Picrochole.Data.Orders
  ( Order
  , module Picrochole.Data.Structs.Register
  , readOrders
  , writeOrders
  ) where

import Data.Aeson ( eitherDecodeFileStrict, encodeFile )
import Data.Either ( partitionEithers )
import Data.Text ( Text )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Picrochole.Data.Atlas ( CellKey(..) )
import Picrochole.Data.Base ( UnitKey(..) )
import Picrochole.Data.Structs.Register
import qualified Picrochole.JSON.Orders as J

-- | Ordre de l'état-major à un subordonné.
type Order = CellKey

-- | Construit une instance de `Register Order` à partir d'un fichier JSON.
readOrders :: FilePath -> IO (Either String (Register Order))
readOrders fp = do
  mxs <- eitherDecodeFileStrict fp
  case mxs of
    Left m -> return (Left m)
    Right xs -> do
      case go xs of
        Left m -> return (Left m)
        Right vec -> return (Right (fromVector vec))

  where

    go :: Vector J.Order -> Either String (Vector (Msg Order))
    go vec = case lefts of
      [] -> Right (V.fromList rights)
      (m:_) -> Left m

      where

        tmp = fmap readOrder (V.toList vec)
        (lefts, rights) = partitionEithers tmp

-- | Crée une instance de `Msg Order` à partir de paramètres lus dans un JSON.
readOrder :: J.Order -> Either String (Msg Order)
readOrder o = if J.received o < J.sent o
              then Left ("got a malformed message which has been received before being sent\n" ++ show o)
              else Right (Msg { header = Header { from = UK (J.from o)
                                                , to = UK (J.to o)
                                                , sent = J.sent o
                                                , received = J.received o
                                                }
                              , content = CK (J.content o)
                              })

-- | Enregistre l'instance de `Register Order` dans le fichier indiqué.
writeOrders :: FilePath -> Register Order -> IO ()
writeOrders fp x = encodeFile fp (fmap showOrder (toVector x))

-- | Convertit une instance de `Msg Order` en une structure prête à être
-- sérialisée en JSON.
showOrder :: Msg Order -> J.Order
showOrder o = J.Order { J.from = rawUK (from . header $ o)
                      , J.to = rawUK (to . header $ o)
                      , J.sent = sent . header $ o
                      , J.received = received . header $ o
                      , J.content = rawCK (content o)
                      }

  where

    rawCK :: CellKey -> Int
    rawCK (CK x) = x

    rawUK :: UnitKey -> Text
    rawUK (UK x) = x
