{- |
   Module      : Picrochole.Data.Structs.XsMap
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Implémentation naïve d'un dictionnaire pouvant être requêté soit par une clef
unique, soit par un lieu. Plusieurs valeurs peuvent être associées à un même
lieu.
-}

module Picrochole.Data.Structs.XsMap
  ( XsMap
  , module Picrochole.Data.Structs.Bag
  , fromMap
  , toMap
  , toList
  , locationKeys
  , elemKeys
  , lookupLocation
  , lookupLocationToken
  , lookupLocationContent
  , lookupKey
  , insertToken
  , insertKey
  , deleteKey
  ) where

import Data.Maybe ( mapMaybe )
import Data.Either ( rights )
import qualified Data.Map as M -- FIXME : IntMap ?
import Data.Set ( Set )
import qualified Data.Set as S

import Picrochole.Data.Structs.Bag hiding ( fromList, toList )
import qualified Picrochole.Data.Structs.Bag as B

-- | Dictionnaire pouvant être requêté soit par un lieu (`k1`), soit par une
-- clef unique (`k2`). Plusieurs valeurs peuvent être associées à un même lieu.
data XsMap k1 k2 a b = XsMap { content :: M.Map k2 b
                             , locs :: M.Map k1 (Either a (S.Set k2))
                             , k1_ :: b -> k1
                             }

instance (Show k1, Ord k2, Show a, Show b) => Show (XsMap k1 k2 a b) where
  show ms = show (toMap ms)

instance Foldable (XsMap k1 k2 a) where
  foldr f z xs = foldr f z (content xs)

-- | Construit un dictionnaire à partir d'un dictionnaire simple.
fromMap :: Ord k2
        => (b -> k1)
        -> (b -> k2)
        -> M.Map k1 (Either a [b])
        -> XsMap k1 k2 a b
fromMap f g m = XsMap { content = content'
                      , locs = locs'
                      , k1_ = f
                      }
  where
    elems = (concat . rights . M.elems) m
    content' = M.fromList (fmap (\ x -> (g x, x)) elems)
    locs' = M.map go m

    go (Left x) = Left x
    go (Right xs) = Right (S.fromList $ fmap g xs)

-- | Transforme le dictionnaire en un dictionnaire simple.
toMap :: Ord k2 => XsMap k1 k2 a b -> M.Map k1 (Either a (Bag b))
toMap ms = M.map go (locs ms)
  where
    go (Left x) = Left x
    go (Right xs) = Right (B.fromList $ mapMaybe f (S.toList xs))

    f k = M.lookup k (content ms)

-- | Transforme le dictionnaire en une liste de paires clef / valeur.
toList :: Ord k2 => XsMap k1 k2 a b -> [(k1, Either a (Bag b))]
toList = M.toList . toMap

-- | Renvoie les clefs de tous les emplacements de la structure.
locationKeys :: Ord k2 => XsMap k1 k2 a b -> Set k1
locationKeys xs = M.keysSet (locs xs)

-- | Renvoie les clefs de tous les éléments de la structure.
elemKeys :: Ord k2 => XsMap k1 k2 a b -> Set k2
elemKeys xs = M.keysSet (content xs)

-- | Renvoie tous les éléments (marqueur ou contenu) associés au lieu donné.
lookupLocation :: (Ord k1, Ord k2) => k1 -> XsMap k1 k2 a b -> Either a (Bag b)
lookupLocation here ms = case M.lookup here (locs ms) of
  Nothing -> Right emptyBag
  Just (Left x) -> Left x
  Just (Right ls) -> Right
                     $ B.fromList
                     $ mapMaybe (\ k -> lookupKey k ms) (S.toList ls)

-- | Renvoie le marqueur éventuel associé au lieu donné.
lookupLocationToken :: (Ord k1, Ord k2) => k1 -> XsMap k1 k2 a b -> Maybe a
lookupLocationToken here ms = case lookupLocation here ms of
  Left x -> Just x
  Right _ -> Nothing

-- | Renvoie tout le contenu associé au lieu donné.
lookupLocationContent :: (Ord k1, Ord k2) => k1 -> XsMap k1 k2 a b -> Bag b
lookupLocationContent here ms = case lookupLocation here ms of
  Left _ -> emptyBag
  Right xs -> xs

-- | Renvoie l'élément associé à la clef donnée.
lookupKey :: (Ord k1, Ord k2) => k2 -> XsMap k1 k2 a b -> Maybe b
lookupKey k ms = M.lookup k (content ms)

-- | Insère un marqueur au lieu indiqué.
insertToken :: (Ord k1, Ord k2) => k1 -> a -> XsMap k1 k2 a b -> XsMap k1 k2 a b
insertToken ck x ms = ms { locs = ls' }
  where
    ls = locs ms
    ls' = M.insert ck new ls
    new = case M.lookup ck ls of
            Nothing -> Left x
            Just (Left _) -> Left x
            Just (Right s) -> Right s

-- | Insère un élément dans le dictionnaire.
insertKey :: (Ord k1, Ord k2)
          => k2
          -> b
          -> XsMap k1 k2 a b
          -> XsMap k1 k2 a b
insertKey k x' ms = ms { content = c'
                       , locs = ls''
                       }
  where
    c' = M.insert k x' (content ms)
    here = (k1_ ms) x'
    ls' = rmLoc k ms
    ls'' = case M.lookup here ls' of
      Nothing -> M.insert here (Right $ S.singleton k) ls'
      Just (Left _) -> M.insert here (Right $ S.singleton k) ls'
      Just (Right keys) -> M.insert here (Right $ S.insert k keys) ls'

-- | Supprime un élément dans le dictionnaire.
deleteKey :: (Ord k1, Ord k2)
          => k2
          -> XsMap k1 k2 a b
          -> XsMap k1 k2 a b
deleteKey k ms = ms { content = M.delete k (content ms)
                    , locs = rmLoc k ms
                    }

-- | Supprime un élément de `locs`.
rmLoc :: (Ord k1, Ord k2)
      => k2
      -> XsMap k1 k2 a b
      -> M.Map k1 (Either a (S.Set k2))
rmLoc k ms = case M.lookup k (content ms) of
  Nothing -> ls
  Just x -> case M.lookup (k1_ ms x) ls of
    Nothing -> error "trying to use a malformed XsMap"
    Just (Left _) -> error "trying to use a malformed XsMap"
    Just (Right s) -> if null s'
                      then M.delete (k1_ ms x) ls
                      else M.insert (k1_ ms x) (Right s') ls
      where
        s' = S.delete k s
  where
    ls = locs ms
