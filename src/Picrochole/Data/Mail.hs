{- |
   Module      : Picrochole.Data.Mail
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les messages que les unités s'échangent.
-}

module Picrochole.Data.Mail
  ( Post(..)
  , Header(..)
  , Register
  , Report
  , organize
  , toVector
  , mkHeader
  , sendReport
  , sendOrder
  , dateLastReportSent
  , getLastReports
  , getLastOrder
  ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )
import Data.Sequence ( Seq(..), (|>) )
import qualified Data.Sequence as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Picrochole.Data.Base
import Picrochole.Data.Board

-- | Identifiant (censément unique) d'un message.
type MsgId = Int

-- | Ensemble des messages de type `a` échangés entre les unités.
data Register a = Register { senders :: Map UnitKey (Map UnitKey (Seq MsgId))
                           , receivers :: Map UnitKey (Map UnitKey (Seq MsgId))
                           , messages :: IntMap (Header, a)
                           , last_ :: Int
                           }
  deriving Show

-- | Les messages que les unités s'échangent.
data Post = Post { reports :: Register Report
                 , orders :: Register Order
                 }
  deriving Show

-- | En-tête d'un message.
data Header = Header { from :: UnitKey
                     , to :: UnitKey
                     , sent :: TurnCount
                     , received :: TurnCount
                     }
  deriving Show

-- | Rapport d'une unité à son état-major.
type Report = [(CellKey, CellContent)]

-- | Ordre de l'état-major à un subordonné.
type Order = CellKey

-- | Organise un vecteur de messages en un registre.
organize :: Vector (Header, a) -> Register a
organize xs = foldr go zero xs
  where
    zero = Register { senders = M.empty
                    , receivers = M.empty
                    , messages = IM.empty
                    , last_ = 0
                    }
    go :: (Header, a) -> Register a -> Register a
    go (header, x) register = send header x register

-- | Transforme un registre en un vecteur de messages.
toVector :: Register a -> Vector (Header, a)
toVector reg = V.fromList (fmap snd (IM.toList (messages reg)))

-- | Construit un en-tête de message. Cette fonction doit être préférée à la
-- construction "à la main" d'un en-tête.
mkHeader :: TurnCount -> UnitKey -> UnitKey -> Int -> Header
mkHeader tCount sender receiver d = Header { from = sender
                                           , to = receiver
                                           , sent = tCount
                                           , received = eta tCount d
                                           }

-- | Calcule la date de réception d'un message.
eta :: TurnCount -> Int -> TurnCount
eta t dist = if dist < 0
             then error "got a negative distance" -- HACK
             else t + dt
  where
    dt = dist `div` 3

-- | Ajoute un message à un registre.
send :: Header -> a -> Register a -> Register a
send header x register = Register { senders = senders'
                                  , receivers = receivers'
                                  , messages = messages'
                                  , last_ = idx
                                  }
  where
    idx = (last_ register) + 1
    senders' = nestedInsert (from header) (to header) idx (senders register)
    receivers' = nestedInsert (to header) (from header) idx (receivers register)
    messages' = IM.insert idx (header, x) (messages register)

-- | Envoie un rapport.
sendReport :: Header -> Report -> Post -> Post
sendReport header report post = post { reports = reports' }
  where
    reports' = send header report (reports post)

-- | Envoie un ordre.
sendOrder :: Header -> Order -> Post -> Post
sendOrder header order post = post { orders = orders' }
  where
    orders' = send header order (orders post)

-- | Insère une valeur dans un dictionnaire de dictionnaire.
nestedInsert :: Ord k
             => k
             -> k
             -> a
             -> Map k (Map k (Seq a))
             -> Map k (Map k (Seq a))
nestedInsert k1 k2 x' xsss = M.insert k1 xs' xsss
  where
    xs' = case M.lookup k1 xsss of
      Just xss -> case M.lookup k2 xss of
        Just xs -> M.insert k2 (xs |> x') xss
        Nothing -> M.insert k2 (S.singleton x') xss
      Nothing -> M.singleton k2 (S.singleton x')

-- | Indique à quelle date l'unité a envoyé son dernier rapport.
dateLastReportSent :: Post -> UnitKey -> UnitKey -> Maybe TurnCount
dateLastReportSent post ukey hq = do
  addressee <- M.lookup ukey (senders (reports post))
  msgIds <- M.lookup hq addressee
  idx <- takeR msgIds
  (header, _) <- IM.lookup idx (messages (reports post))
  return (sent header)

-- | Renvoie le dernier élément d'une séquence.
takeR :: Seq a -> Maybe a
takeR Empty = Nothing
takeR (_ :|> x) = Just x

-- | Renvoie le dernier rapport envoyé par chaque subordonné.
getLastReports :: UnitKey -> Post -> [(Header, Report)]
getLastReports ukey post = case M.lookup ukey (receivers (reports post)) of
  Nothing -> []
  Just xss -> catMaybes (fmap go (M.elems xss))

  where

    go :: Seq MsgId -> Maybe (Header, Report)
    go msgIds = do
      idx <- takeR msgIds
      IM.lookup idx (messages (reports post))

-- | Renvoie le dernier ordre envoyé par le QG.
getLastOrder :: UnitKey -> UnitKey -> Post -> Maybe (Header, Order)
getLastOrder ukey hq post = do
  got <- M.lookup ukey (receivers (orders post))
  msgIds <- M.lookup hq got
  idx <- takeR msgIds
  IM.lookup idx (messages (orders post))
