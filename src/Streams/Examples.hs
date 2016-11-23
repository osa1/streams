{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}

-- | These examples can't be optimized by GHC because stream function
-- definitions  are completely opaque to GHC.

module Streams.Examples where

--------------------------------------------------------------------------------

import qualified Streams.Fold as Fold
import qualified Streams.Unfold as Unfold

import Data.List (foldl')
import Prelude hiding (filter, map, take, zip)
import qualified Prelude

--------------------------------------------------------------------------------

data StreamLib = forall s . StreamLib
  { append   :: forall a . s a -> s a -> s a
  , cons     :: forall a . a -> s a -> s a
  , filter   :: forall a . (a -> Bool) -> s a -> s a
  , flatten  :: forall a . s (s a) -> s a
  , fromList :: forall a . [a] -> s a
  , fold     :: forall a b . (b -> a -> b) -> b -> s a -> b
  , map      :: forall a b . (a -> b) -> s a -> s b
  , snoc     :: forall a . s a -> a -> s a
  , take     :: forall a . Int -> s a -> s a
  , toList   :: forall a . s a -> [a]
  , zip      :: forall a b . s a -> s b -> s (a, b)
  }

listLib :: StreamLib
listLib = StreamLib
  { append      = (++)
  , cons        = (:)
  , filter      = Prelude.filter
  , flatten     = concat
  , fromList    = id
  , fold        = foldl'
  , map         = Prelude.map
  , snoc        = \l a -> l ++ [a]
  , take        = Prelude.take
  , toList      = id
  , zip         = Prelude.zip
  }

foldLib :: StreamLib
foldLib = StreamLib
  { append   = Fold.append
  , cons     = Fold.cons
  , filter   = Fold.filter
  , flatten  = Fold.flatten
  , fromList = Fold.fromList
  , fold     = Fold.fold
  , map      = Fold.map
  , snoc     = Fold.snoc
  , take     = undefined -- Fold.take
  , toList   = Fold.toList
  , zip      = undefined -- Fold.zip
  }

unfoldLib :: StreamLib
unfoldLib = StreamLib
  { append   = Unfold.append
  , cons     = Unfold.cons
  , filter   = Unfold.filter
  , flatten  = Unfold.flatten
  , fold     = Unfold.fold
  , fromList = Unfold.fromList
  , map      = Unfold.map
  , snoc     = Unfold.snoc
  , take     = Unfold.take
  , toList   = Unfold.toList
  , zip      = Unfold.zip
  }

--------------------------------------------------------------------------------

square :: Num a => a -> a
square x = x * x

sum :: StreamLib -> [Int] -> Int
sum StreamLib{ fold, fromList } l =
    fold (+) 0 (fromList l)

sumOfSquares :: StreamLib -> [Int] -> Int
sumOfSquares StreamLib{ map, fold, fromList } l =
    fold (+) 0 . map square $ fromList l

sumOfSquaresEven :: StreamLib -> [Int] -> Int
sumOfSquaresEven StreamLib{ map, fold, filter, fromList } l =
    fold (+) 0 . map square . filter even $ fromList l

cart :: StreamLib -> [Int] -> [Int] -> Int
cart StreamLib{ fold, flatten, map, fromList } l1 l2 =
    fold (+) 0 $
    flatten $
    map (\x -> map (x *) (fromList l2)) $
    fromList l1

maps :: StreamLib -> [Int] -> Int
maps StreamLib{ map, fold, fromList } l =
    fold (+) 0 $
    map (7 *) $
    map (6 *) $
    map (5 *) $
    map (4 *) $
    map (3 *) $
    map (2 *) $
    map (1 *) $
    fromList l

filters :: StreamLib -> [Int] -> Int
filters StreamLib{ filter, fold, fromList} l =
     fold (+) 0 $
     filter (> 7) $
     filter (> 6) $
     filter (> 5) $
     filter (> 4) $
     filter (> 3) $
     filter (> 2) $
     filter (> 1) $
     fromList l

dotProduct :: StreamLib -> [Int] -> [Int] -> Int
dotProduct StreamLib{ map, fold, fromList, zip } l1 l2 =
    fold (+) 0 $
    map (uncurry (*)) $
    zip (fromList l1) (fromList l2)

flatMapAfterZipWith :: StreamLib -> [Int] -> [Int] -> Int
flatMapAfterZipWith StreamLib{ fold, flatten, map, zip, fromList } l1 l2 =
    fold (+) 0 $
    flatten $
    map (\x -> map (x +) (fromList l2)) $
    map (uncurry (+)) $
    zip (fromList l1) (fromList l1)

zipWithAfterFlatMap :: StreamLib -> [Int] -> [Int] -> Int
zipWithAfterFlatMap StreamLib{ fold, map, zip, fromList, flatten } l1 l2 =
    fold (+) 0 $
    map (uncurry (+)) $
    zip (fromList l1) $
    flatten $
    map (\x -> map (x +) (fromList l2)) $
    fromList l1

flatMapTake :: StreamLib -> [Int] -> [Int] -> Int
flatMapTake StreamLib{ fold, take, flatten, map, fromList } l1 l2 =
    fold (+) 0 $
    take 20000000 $
    flatten $
    map (\x -> map (x +) (fromList l2)) $
    fromList l1
