{- |
   Module      : Picrochole.Utils.XsMap
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Implémentation naïve d'un dictionnaire pouvant être requêté soit par une clef
unique, soit par un lieu. Plusieurs valeurs peuvent être associées à un même
lieu.
-}

module Picrochole.Utils.XsMap
  ( XsMap
  , HasLocation(..)
  , empty
  , fromList
  , lookupLocation
  , lookupKey
  , insertKey
  , deleteKey
  , foldXsMap
  ) where

import Data.Maybe ( catMaybes )
import qualified Data.Map as M -- FIXME : IntMap ?
import qualified Data.Set as S

-- | Classe pour les types contenant une clef de lieu.
class HasLocation a where
  location :: a k1 -> k1
-- FIXME : supprimer cette classe et faire intervenir directement `CellKey`.

-- | Dictionnaire pouvant être requêté soit par un lieu (clef k1), soit
-- par une clef unique (clef k2). Plusieurs valeurs peuvent être
-- associées à un même lieu.
data XsMap k1 k2 a = XsMap { content :: M.Map k2 (a k1)
                           , locs :: M.Map k1 (S.Set k2)
                           }
  deriving Show

-- | Effectue un fold sur les éléments de la structure.
foldXsMap :: (a k1 -> b -> b) -> b -> XsMap k1 k2 a -> b
foldXsMap f z xs = foldr f z (content xs)

{- FIXME : pourra être remplacé par quelque chose du genre :
instance Foldable (XsMap k) where
  foldr f z xs = foldr f z (content xs)
-}

-- | Un dictionnaire vide.
empty :: XsMap k1 k2 a
empty = XsMap { content = M.empty
              , locs = M.empty
              }

-- | Construit un dictionnaire à partir d'une liste de paires clef / valeur.
fromList :: (HasLocation a, Ord k1, Ord k2) => [(k2, a k1)] -> XsMap k1 k2 a
fromList xs = foldr go empty xs
  where
    go (k, x) ms = insertKey k x ms

-- | Renvoie tous les éléments associés au lieu donné.
lookupLocation :: (Ord k1, Ord k2) => k1 -> XsMap k1 k2 a -> [a k1]
lookupLocation here ms = case M.lookup here (locs ms) of
  Nothing -> []
  Just ls -> catMaybes (fmap (\ k -> lookupKey k ms) (S.toList ls))

-- | Renvoie l'élément associé à la clef donnée.
lookupKey :: Ord k2 => k2 -> XsMap k1 k2 a -> Maybe (a k1)
lookupKey k ms = M.lookup k (content ms)

-- | Insère un élément dans le dictionnaire.
insertKey :: (HasLocation a, Ord k1, Ord k2)
          => k2
          -> a k1
          -> XsMap k1 k2 a
          -> XsMap k1 k2 a
insertKey k x' ms = ms { content = c'
                       , locs = ls''
                       }
  where
    c' = M.insert k x' (content ms)
    here = location x'
    ls = locs ms
    ls' = case M.lookup k (content ms) of
      Nothing -> ls
      Just x -> case M.lookup (location x) ls of
        Nothing -> error "trying to use a malformed XsMap"
        Just s -> if null s'
                  then M.delete (location x) ls
                  else M.insert (location x) s' ls
          where
            s' = S.delete k s
    ls'' = case M.lookup here ls' of
      Nothing -> M.insert here (S.singleton k) ls'
      Just keys -> M.insert here (S.insert k keys) ls'

-- | Supprime un élément dans le dictionnaire.
deleteKey :: (HasLocation a, Ord k1, Ord k2)
          => k2
          -> XsMap k1 k2 a
          -> XsMap k1 k2 a
deleteKey k ms = ms { content = c'
                    , locs = ls'
                    }
  where
    c' = M.delete k (content ms)
    -- FIXME : code dupliqué avec `insertKey`.
    ls = locs ms
    ls' = case M.lookup k (content ms) of
      Nothing -> ls
      Just x -> case M.lookup (location x) ls of
        Nothing -> error "trying to use a malformed XsMap"
        Just s -> if null s'
                  then M.delete (location x) ls
                  else M.insert (location x) s' ls
          where
            s' = S.delete k s