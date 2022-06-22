{- |
   Module      : Picrochole.Data.Mail
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les messages que les unités s'échangent.
-}

module Picrochole.Data.Mail
  ( Post
  , Header
  , Report
  , from
  , to
  , sent
  , received
  , mkHeader
  , sendReport
  , dateLastReportSent
  ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Sequence ( Seq(..), (|>) )
import qualified Data.Sequence as S
import Picrochole.Data.Base
import Picrochole.Data.Board

-- | Identifiant (censément unique) d'un message.
type MsgId = Int

-- | Les messages que les unités s'échangent.
data Post = Post { senders :: Map UnitKey (Map UnitKey (Seq MsgId))
                 , receivers :: Map UnitKey (Map UnitKey (Seq MsgId))
                 , messages :: IntMap (Header, Report)
                 , last_ :: Int
                 }
  deriving Show

-- | En-tête d'un message.
data Header = Header { from_ :: UnitKey
                     , to_ :: UnitKey
                     , sent_ :: TurnCount
                     , received_ :: TurnCount
                     }
  deriving Show

-- | Rapport d'une unité à son état-major.
type Report = [Cell]

-- | Indique l'expéditeur du message.
from :: Header -> UnitKey
from = from_

-- | Indique le destinataire du message.
to :: Header -> UnitKey
to = to_

-- | Indique la date d'envoi du message.
sent :: Header -> TurnCount
sent = sent_

-- | Indique la date de réception du message.
received :: Header -> TurnCount
received = received_

-- | Construit un en-tête de message.
mkHeader :: TurnCount -> UnitKey -> UnitKey -> Int -> Header
mkHeader tCount sender receiver d = Header { from_ = sender
                                           , to_ = receiver
                                           , sent_ = tCount
                                           , received_ = eta tCount d
                                           }

-- | Calcule la date de réception d'un message.
eta :: TurnCount -> Int -> TurnCount
eta t dist = if dist < 0
             then error "got a negative distance" -- HACK
             else t + dt
  where
    dt = dist `div` 3

-- | Envoie un message.
sendReport :: Header -> Report -> Post -> Post
sendReport header report post = Post { senders = senders'
                                     , receivers = receivers'
                                     , messages = messages'
                                     , last_ = idx
                                     }
  where
    idx = (last_ post) + 1
    senders' = nestedInsert (from header) (to header) idx (senders post)
    receivers' = nestedInsert (to header) (from header) idx (receivers post)
    messages' = IM.insert idx (header, report) (messages post)

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
  addressee <- M.lookup ukey (senders post)
  msgIds <- M.lookup hq addressee
  idx <- takeR msgIds
  (header, _) <- IM.lookup idx (messages post)
  return (sent header)

-- | Renvoie le dernier élément d'une séquence.
takeR :: Seq a -> Maybe a
takeR Empty = Nothing
takeR (_ :|> x) = Just x
