{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import Test.QuickCheck

import qualified Streams.Fold as Fold
import qualified Streams.Unfold as Unfold

import Prelude hiding (filter, map)

data StreamLib = forall s . StreamLib
  { fromList :: forall a . [a] -> s a
  , fold     :: forall a b . (b -> a -> b) -> b -> s a -> b
  , map      :: forall a b . (a -> b) -> s a -> s b
  , filter   :: forall a . (a -> Bool) -> s a -> s a
  , snoc     :: forall a . s a -> a -> s a
  , cons     :: forall a . a -> s a -> s a
  , append   :: forall a . s a -> s a -> s a
  , flatten  :: forall a . s (s a) -> s a
  , toList   :: forall a . s a -> [a]
  }

foldLib :: StreamLib
foldLib = StreamLib
  { fromList = Fold.fromList
  , fold     = Fold.fold
  , map      = Fold.map
  , filter   = Fold.filter
  , snoc     = Fold.snoc
  , cons     = Fold.cons
  , append   = Fold.append
  , flatten  = Fold.flatten
  , toList   = Fold.toList
  }

unfoldLib :: StreamLib
unfoldLib = StreamLib
  { fromList = Unfold.fromList
  , fold     = Unfold.fold
  , map      = Unfold.map
  , filter   = Unfold.filter
  , snoc     = Unfold.snoc
  , cons     = Unfold.cons
  , append   = Unfold.append
  , flatten  = Unfold.flatten
  , toList   = Unfold.toList
  }

fromList_toList :: StreamLib -> Gen Property
fromList_toList StreamLib{ fromList, toList } = do
    lst :: [Int] <- arbitrary
    return (toList (fromList lst) === lst)

fold_len :: StreamLib -> Gen Property
fold_len StreamLib{ fold, fromList } = do
    lst :: [Int] <- arbitrary
    return (fold (\l _ -> l + 1) 0 (fromList lst) === length lst)

main :: IO ()
main = do
    quickCheck (fromList_toList foldLib)
    quickCheck (fromList_toList unfoldLib)
    quickCheck (fold_len foldLib)
    quickCheck (fold_len unfoldLib)
