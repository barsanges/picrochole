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
  , lookupLocationToken
  , lookupLocationContent
  , lookupKey
  , insertToken
  , insertKey
  , deleteKey
  ) where

import Data.Maybe ( mapMaybe )
import qualified Data.Map as M -- FIXME : IntMap ?
import qualified Data.Set as S
import Picrochole.Data.Utils.HexGrid ( CellKey )

-- | Classe pour les types contenant une clef de lieu.
class HasLocation a where
  location :: a -> CellKey

-- | Dictionnaire pouvant être requêté soit par un lieu (clef de type
-- `CellKey`), soit par une clef unique. Plusieurs valeurs peuvent être
-- associées à un même lieu.
data XsMap k a b = XsMap { content :: M.Map k b
                         , locs :: M.Map CellKey (Either a (S.Set k))
                         }
  deriving Show

instance Foldable (XsMap k a) where
  foldr f z xs = foldr f z (content xs)

-- | Un dictionnaire vide.
empty :: XsMap k a b
empty = XsMap { content = M.empty
              , locs = M.empty
              }

-- | Construit un dictionnaire à partir d'une liste de paires clef / valeur.
fromList :: (HasLocation b, Ord k) => [(k, b)] -> XsMap k a b
fromList xs = foldr go empty xs
  where
    go (k, x) ms = insertKey k x ms

-- | Renvoie tous les éléments (marqueur ou contenu) associés au lieu donné.
lookupLocation :: Ord k => CellKey -> XsMap k a b -> Either a [b]
lookupLocation here ms = case M.lookup here (locs ms) of
  Nothing -> Right []
  Just (Left x) -> Left x
  Just (Right ls) -> Right $ mapMaybe (\ k -> lookupKey k ms) (S.toList ls)

-- | Renvoie le marqueur éventuel associé au lieu donné.
lookupLocationToken :: Ord k => CellKey -> XsMap k a b -> Maybe a
lookupLocationToken here ms = case lookupLocation here ms of
  Left x -> Just x
  Right _ -> Nothing

-- | Renvoie tout le contenu associé au lieu donné.
lookupLocationContent :: Ord k => CellKey -> XsMap k a b -> [b]
lookupLocationContent here ms = case lookupLocation here ms of
  Left _ -> []
  Right xs -> xs

-- | Renvoie l'élément associé à la clef donnée.
lookupKey :: Ord k => k -> XsMap k a b -> Maybe b
lookupKey k ms = M.lookup k (content ms)

-- | Insère un marqueur au lieu indiqué.
insertToken :: CellKey -> a -> XsMap k a b -> XsMap k a b
insertToken ck x ms = ms { locs = ls' }
  where
    ls = locs ms
    ls' = M.insert ck new ls
    new = case M.lookup ck ls of
            Nothing -> Left x
            Just (Left _) -> Left x
            Just (Right s) -> Right s

-- | Insère un élément dans le dictionnaire.
insertKey :: (HasLocation b, Ord k)
          => k
          -> b
          -> XsMap k a b
          -> XsMap k a b
insertKey k x' ms = ms { content = c'
                       , locs = ls''
                       }
  where
    c' = M.insert k x' (content ms)
    here = location x'
    ls' = rmLoc k ms
    ls'' = case M.lookup here ls' of
      Nothing -> M.insert here (Right $ S.singleton k) ls'
      Just (Left _) -> M.insert here (Right $ S.singleton k) ls'
      Just (Right keys) -> M.insert here (Right $ S.insert k keys) ls'

-- | Supprime un élément dans le dictionnaire.
deleteKey :: (HasLocation b, Ord k)
          => k
          -> XsMap k a b
          -> XsMap k a b
deleteKey k ms = ms { content = M.delete k (content ms)
                    , locs = rmLoc k ms
                    }

-- | Supprime un élément de `locs`.
rmLoc :: (HasLocation b, Ord k)
      => k
      -> XsMap k a b
      -> M.Map CellKey (Either a (S.Set k))
rmLoc k ms = case M.lookup k (content ms) of
  Nothing -> ls
  Just x -> case M.lookup (location x) ls of
    Nothing -> error "trying to use a malformed XsMap"
    Just (Left _) -> error "trying to use a malformed XsMap"
    Just (Right s) -> if null s'
                      then M.delete (location x) ls
                      else M.insert (location x) (Right s') ls
      where
        s' = S.delete k s
  where
    ls = locs ms
