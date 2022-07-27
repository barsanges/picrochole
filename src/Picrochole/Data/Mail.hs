{- |
   Module      : Picrochole.Data.Mail
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Les messages que les unités s'échangent.
-}

module Picrochole.Data.Mail
  ( Register
  , Header(..)
  , Report
  , Order
  , fromVector
  , toVector
  , mkHeader
  , send
  , lastSent
  , lastSent'
  , lastReceived
  , lastReceived'
  ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Map ( Map )
import qualified Data.Map as M
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

-- | En-tête d'un message.
data Header = Header { from :: UnitKey
                     , to :: UnitKey
                     , sent :: TurnCount
                     , received :: TurnCount
                     }
  deriving Show

-- | Rapport d'une unité à son état-major.
type Report = Map CellKey CellContent

-- | Ordre de l'état-major à un subordonné.
type Order = CellKey

-- | Construit un registre à partir d'un vecteur de messages.
fromVector :: Vector (Header, a) -> Register a
fromVector xs = foldr go zero xs
  where
    zero = Register { senders = M.empty
                    , receivers = M.empty
                    , messages = IM.empty
                    , last_ = 0
                    }
    go :: (Header, a) -> Register a -> Register a
    go (header, x) register = send header x register

-- | Convertit un registre en un vecteur de messages.
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

-- | Renvoie le dernier message envoyé par l'unité `x` à un correspondant `y`.
lastSent :: Register a -> UnitKey -> UnitKey -> Maybe (Header, a)
lastSent reg x y = do
  addressee <- M.lookup x (senders reg)
  msgIds <- M.lookup y addressee
  idx <- takeFirstR (const True) msgIds
  IM.lookup idx (messages reg)

-- | Renvoie le dernier message envoyé par l'unité `x` à chacun de ses
-- correspondants.
lastSent' :: Register a -> UnitKey -> Map UnitKey (Header, a)
lastSent' reg x = case M.lookup x (senders reg) of
  Nothing -> M.empty
  Just ms -> mmapMaybe go ms
  where
    go sq = do
      idx <- takeFirstR (const True) sq
      IM.lookup idx (messages reg)

-- | Renvoie le dernier message reçu par l'unité `x` d'un correspondant `y`.
lastReceived :: TurnCount
             -> Register a
             -> UnitKey
             -> UnitKey
             -> Maybe (Header, a)
lastReceived tcount reg x y = do
  got <- M.lookup x (receivers reg)
  msgIds <- M.lookup y got
  idx <- takeFirstR (hasBeenReceived tcount reg) msgIds
  IM.lookup idx (messages reg)

-- | Renvoie le dernier message reçu par l'unité `x` de chacun de ses
-- correspondants.
lastReceived' :: TurnCount
              -> Register a
              -> UnitKey
              -> Map UnitKey (Header, a)
lastReceived' tcount reg x = case M.lookup x (receivers reg) of
  Nothing -> M.empty
  Just ms -> mmapMaybe go ms
  where
    go sq = do
      idx <- takeFirstR (hasBeenReceived tcount reg) sq
      IM.lookup idx (messages reg)

-- | Indique si le message identifié par `idx` a déjà été reçu au tour `tcount`.
hasBeenReceived :: TurnCount -> Register a -> MsgId -> Bool
hasBeenReceived tcount reg idx = case IM.lookup idx (messages reg) of
  Nothing -> False
  Just (h, _) -> received h <= tcount

-- | Renvoie le premier élément (en partant de la droite) qui satisfait un
-- prédicat.
takeFirstR :: (a -> Bool) -> Seq a -> Maybe a
takeFirstR _ Empty = Nothing
takeFirstR f (xs :|> x) = if f x
                          then Just x
                          else takeFirstR f xs

-- | Equivalent de `map` qui permet d'éliminer des éléments.
mmapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mmapMaybe f xs = M.foldrWithKey go M.empty xs
  where
    go k x tmp = case f x of
      Nothing -> tmp
      Just y -> M.insert k y tmp
