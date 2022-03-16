{- |
   Module      : Picrochole.Utils.MultiMap
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Implémentation naïve d'un dictionnaire pouvant être requêté soit par une clef
unique, soit par un lieu.
-}

module Picrochole.Utils.MultiMap
  ( MultiMap
  , HasLocation(..)
  , empty
  , fromList
  , lookupLocation
  , lookupKey
  , insertKey
  ) where

import Data.Maybe ( catMaybes )
import qualified Data.Map as M -- FIXME : IntMap ?
import qualified Data.Set as S
import Picrochole.Data.Keys

-- | Dictionnaire pouvant être requêté soit par une clef unique, soit par un
-- lieu.
data MultiMap k a = MMap { content :: M.Map k a
                         , locs :: M.Map LocationKey (S.Set k)
                         }
  deriving Show

-- | Classe pour les types contenant une clef de lieu.
class HasLocation a where
  getLocation :: a -> LocationKey

-- | Un dictionnaire vide.
empty :: MultiMap k a
empty = MMap { content = M.empty
             , locs = M.empty
             }

-- | Construit un dictionnaire à partir d'une liste de paires clef / valeur.
fromList :: (HasLocation a, Ord k) => [(k, a)] -> MultiMap k a
fromList xs = foldr go empty xs
  where
    go (k, x) ms = insertKey k x ms

-- | Renvoie tous les éléments associés au lieu donné.
lookupLocation :: Ord k => LocationKey -> MultiMap k a -> [a]
lookupLocation here ms = case M.lookup here (locs ms) of
  Nothing -> []
  Just ls -> catMaybes (fmap (\ k -> lookupKey k ms) (S.toList ls))

-- | Renvoie l'élément associé à la clef donnée.
lookupKey :: Ord k => k -> MultiMap k a -> Maybe a
lookupKey k ms = M.lookup k (content ms)

-- | Insère un élément dans le dictionnaire.
insertKey :: (HasLocation a, Ord k) => k -> a -> MultiMap k a -> MultiMap k a
insertKey k x' ms = ms { content = c'
                       , locs = ls''
                       }
  where
    c' = M.insert k x' (content ms)
    here = getLocation x'
    ls = locs ms
    ls' = case M.lookup k (content ms) of
      Nothing -> ls
      Just x -> case M.lookup (getLocation x) ls of
        Nothing -> error "trying to use a malformed MultiMap"
        Just s -> if null s'
                  then M.delete (getLocation x) ls
                  else M.insert (getLocation x) s' ls
          where
            s' = S.delete k s
    ls'' = case M.lookup here ls' of
      Nothing -> M.insert here (S.singleton k) ls'
      Just keys -> M.insert here (S.insert k keys) ls'
