{- |
   Module      : Picrochole.Utils.XMap
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Implémentation naïve d'un dictionnaire pouvant être requêté soit par une clef
unique, soit par un lieu. Une seule valeur peut être associée à un lieu.
-}

module Picrochole.Utils.XMap
  ( XMap
  , empty
  , fromList
  , lookupLocation
  , lookupKey
  , insertKey
  ) where

import qualified Data.Map as M -- FIXME : IntMap ?
import Picrochole.Data.Keys

-- | Dictionnaire pouvant être requêté soit par une clef unique, soit par un
-- lieu. Une seule valeur peut être associée à un lieu.
data XMap k a = XMap { content :: M.Map k a
                     , locs :: M.Map LocationKey k
                     }
  deriving Show

instance Foldable (XMap k) where
  foldr f y xs = foldr f y (content xs)

-- | Un dictionnaire vide.
empty :: XMap k a
empty = XMap { content = M.empty
             , locs = M.empty
             }

-- | Construit un dictionnaire à partir d'une liste de paires clef / valeur.
fromList :: (HasLocation a, Ord k) => [(k, a)] -> XMap k a
fromList xs = foldr go empty xs
  where
    go (k, x) ms = insertKey k x ms

-- | Renvoie l'élément associé au lieu donné.
lookupLocation :: Ord k => LocationKey -> XMap k a -> Maybe a
lookupLocation here ms = do
  loc <- M.lookup here (locs ms)
  M.lookup loc (content ms)

-- | Renvoie l'élément associé à la clef donnée.
lookupKey :: Ord k => k -> XMap k a -> Maybe a
lookupKey k ms = M.lookup k (content ms)

-- | Insère un élément dans le dictionnaire. Renvoie le dictionnaire initial
-- si le lieu est déjà associé à une autre clef que celle fournie.
insertKey :: (HasLocation a, Ord k) => k -> a -> XMap k a -> XMap k a
insertKey k x' ms = case M.lookup here ls' of
  Nothing -> ms { content = M.insert k x' (content ms)
                , locs = M.insert here k ls'
                }
  Just _ -> ms
  where
    here = getLocation x'
    ls = locs ms
    ls' = case M.lookup k (content ms) of
      Nothing -> ls
      Just x -> M.delete (getLocation x) ls
