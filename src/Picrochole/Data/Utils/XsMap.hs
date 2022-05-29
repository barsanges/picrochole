{- |
   Module      : Picrochole.Data.Utils.XsMap
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Implémentation naïve d'un dictionnaire pouvant être requêté soit par une clef
unique, soit par un lieu. Plusieurs valeurs peuvent être associées à un même
lieu.
-}

module Picrochole.Data.Utils.XsMap
  ( XsMap
  , HasLocation(..)
  , empty
  , fromList
  , lookupLocation
  , lookupKey
  , insertKey
  , deleteKey
  ) where

import Data.Maybe ( catMaybes )
import qualified Data.Map as M -- FIXME : IntMap ?
import qualified Data.Set as S
import Picrochole.Data.Utils.HexGrid ( CellKey )

-- | Classe pour les types contenant une clef de lieu.
class HasLocation a where
  location :: a -> CellKey

-- | Dictionnaire pouvant être requêté soit par un lieu (clef de type
-- `CellKey`), soit par une clef unique. Plusieurs valeurs peuvent être
-- associées à un même lieu.
data XsMap k a = XsMap { content :: M.Map k a
                       , locs :: M.Map CellKey (S.Set k)
                       }
  deriving Show

instance Foldable (XsMap k) where
  foldr f z xs = foldr f z (content xs)

-- | Un dictionnaire vide.
empty :: XsMap k a
empty = XsMap { content = M.empty
              , locs = M.empty
              }

-- | Construit un dictionnaire à partir d'une liste de paires clef / valeur.
fromList :: (HasLocation a, Ord k) => [(k, a)] -> XsMap k a
fromList xs = foldr go empty xs
  where
    go (k, x) ms = insertKey k x ms

-- | Renvoie tous les éléments associés au lieu donné.
lookupLocation :: Ord k => CellKey -> XsMap k a -> [a]
lookupLocation here ms = case M.lookup here (locs ms) of
  Nothing -> []
  Just ls -> catMaybes (fmap (\ k -> lookupKey k ms) (S.toList ls))

-- | Renvoie l'élément associé à la clef donnée.
lookupKey :: Ord k => k -> XsMap k a -> Maybe a
lookupKey k ms = M.lookup k (content ms)

-- | Insère un élément dans le dictionnaire.
insertKey :: (HasLocation a, Ord k)
          => k
          -> a
          -> XsMap k a
          -> XsMap k a
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
deleteKey :: (HasLocation a, Ord k)
          => k
          -> XsMap k a
          -> XsMap k a
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
