{- |
   Module      : Picrochole.Data.Structs.Bag
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Un ensemble d'éléments dont l'ordre n'a pas d'importance.
-}

module Picrochole.Data.Structs.Bag
  ( Bag
  , emptyBag
  , fromList
  , toList
  , partition
  ) where

import Data.List ( (\\) )
import qualified Data.List as L

-- | Un ensemble d'éléments dont l'ordre n'a pas d'importance.
newtype Bag a = Bag [a]
    deriving Show

instance Eq a => Eq (Bag a) where
    (Bag xs) == (Bag ys) = (null (xs \\ ys)) && (null (ys \\ xs))

instance Functor Bag where
    fmap f (Bag xs) = Bag (fmap f xs)

instance Foldable Bag where
    foldr f y (Bag xs) = foldr f y xs

-- | Un sac vide.
emptyBag :: Bag a
emptyBag = Bag []

-- | Construit un sac à partir d'une liste d'éléments.
fromList :: [a] -> Bag a
fromList = Bag

-- | Convertit un sac en une liste d'éléments.
toList :: Bag a -> [a]
toList (Bag xs) = xs

-- | Renvoie les éléments du sac qui satisfont et ne satisfont pas le prédicat.
partition :: (a -> Bool) -> Bag a -> (Bag a, Bag a)
partition f (Bag xs) = (Bag ys, Bag zs)
  where
    (ys, zs) = L.partition f xs
