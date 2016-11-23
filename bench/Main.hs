{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Main where

--------------------------------------------------------------------------------

import Control.DeepSeq
import Criterion.Main
import Data.List (foldl')
import GHC.Generics

import qualified Streams.Examples as GE
import qualified Streams.Fold.Examples as FE
import qualified Streams.Unfold.Examples as UE

--------------------------------------------------------------------------------

data Env = Env
  { v    :: ![Int]
  , vHi  :: ![Int]
  , vLo  :: ![Int]
  , vFaZ :: ![Int]
  , vZaF :: ![Int]
  } deriving (Generic, NFData)

setupEnv :: Env
setupEnv = Env
  { v    = map (`mod` 10) [ 0 .. 100000000 ]
  , vHi  = map (`mod` 10) [ 0 ..  10000000 ]
  , vLo  = map (`mod` 10) [ 0 ..        10 ]
  , vFaZ = [ 0 ..     10000 ]
  , vZaF = [ 0 ..  10000000 ]
  }

main :: IO ()
main = defaultMain
  [ env (return setupEnv) $ \lists ->
    bgroup "benchmarks" $
    -- there are not optimized
    (flip map [ ("generic list", GE.listLib),
                ("generic fold", GE.foldLib),
                ("generic unfold", GE.unfoldLib) ] $
      \(impl_name, impl) ->
         bgroup impl_name
           [ bench "sum" (nf (GE.sum impl) (v lists))
           , bench "sumOfSquares" (nf (GE.sumOfSquares impl) (v lists))
           , bench "cart" (nf (GE.cart impl (vHi lists)) (vLo lists))
           , bench "maps" (nf (GE.maps impl) (v lists))
           , bench "filters" (nf (GE.filters impl) (v lists))
           , bench "dotProduct" (nf (GE.dotProduct impl (vHi lists)) (vHi lists))
           , bench "flatMapAfterZipWith" (nf (GE.flatMapAfterZipWith impl (vFaZ lists)) (vFaZ lists))
           , bench "zipWithAfterFlatMap" (nf (GE.zipWithAfterFlatMap impl (vFaZ lists)) (vFaZ lists))
           , bench "flatMapTake" (nf (GE.flatMapTake impl (v lists)) (vLo lists))
           ]) ++

    [ -- slightly better optimization due to known stream functions
      bgroup "list"
      [ bench "sum" (nf sum (v lists))
      , bench "sumOfSquares" (nf sumOfSquares (v lists))
      , bench "cart" (nf (cart (vHi lists)) (vLo lists))
      , bench "maps" (nf maps (v lists))
      , bench "filters" (nf filters (v lists))
      , bench "dotProduct" (nf (dotProduct (vHi lists)) (vHi lists))
      , bench "flatMapAfterZipWith" (nf (flatMapAfterZipWith (vFaZ lists)) (vFaZ lists))
      , bench "zipWithAfterFlatMap" (nf (zipWithAfterFlatMap (vFaZ lists)) (vFaZ lists))
      , bench "flatMapTake" (nf (flatMapTake (v lists)) (vLo lists))
      ]

    , -- slightly better optimization due to known stream functions
      bgroup "fold"
      [ bench "sum" (nf FE.sum (v lists))
      , bench "sumOfSquares" (nf FE.sumOfSquares (v lists))
      , bench "cart" (nf (FE.cart (vHi lists)) (vLo lists))
      , bench "maps" (nf FE.maps (v lists))
      , bench "filters" (nf FE.filters (v lists))
      -- , bench "dotProduct" (nf (FE.dotProduct (vHi lists)) (vHi lists))
      -- , bench "flatMapAfterZipWith" (nf (FE.flatMapAfterZipWith (vFaZ lists)) (vFaZ lists))
      -- , bench "zipWithAfterFlatMap" (nf (FE.zipWithAfterFlatMap (vFaZ lists)) (vFaZ lists))
      -- , bench "flatMapTake" (nf (FE.flatMapTake (v lists)) (vLo lists))
      ]

    , -- slightly better optimization due to known stream functions
      bgroup "unfold"
      [ bench "sum" (nf (foldl' (+) 0) (v lists))
      , bench "sumOfSquares" (nf UE.sumOfSquares (v lists))
      , bench "cart" (nf (UE.cart (vHi lists)) (vLo lists))
      , bench "maps" (nf UE.maps (v lists))
      , bench "filters" (nf UE.filters (v lists))
      , bench "dotProduct" (nf (UE.dotProduct (vHi lists)) (vHi lists))
      , bench "flatMapAfterZipWith" (nf (UE.flatMapAfterZipWith (vFaZ lists)) (vFaZ lists))
      , bench "zipWithAfterFlatMap" (nf (UE.zipWithAfterFlatMap (vFaZ lists)) (vFaZ lists))
      , bench "flatMapTake" (nf (UE.flatMapTake (v lists)) (vLo lists))
      ]
    ]
  ]

square :: Num a => a -> a
square x = x * x

sumOfSquares :: [Int] -> Int
sumOfSquares l =
    foldl' (+) 0 . map square $ l

sumOfSquaresEven :: [Int] -> Int
sumOfSquaresEven l =
    foldl' (+) 0 . map square . filter even $ l

cart :: [Int] -> [Int] -> Int
cart l1 l2 =
    foldl' (+) 0 $
    concat $
    map (\x -> map (x *) l2) $
    l1

maps :: [Int] -> Int
maps l =
    foldl' (+) 0 $
    map (7 *) $
    map (6 *) $
    map (5 *) $
    map (4 *) $
    map (3 *) $
    map (2 *) $
    map (1 *) $
    l

filters :: [Int] -> Int
filters l =
    foldl' (+) 0 $
    filter (> 7) $
    filter (> 6) $
    filter (> 5) $
    filter (> 4) $
    filter (> 3) $
    filter (> 2) $
    filter (> 1) $
    l

dotProduct :: [Int] -> [Int] -> Int
dotProduct l1 l2 =
    foldl' (+) 0 $
    map (uncurry (*)) $
    zip l1 l2

flatMapAfterZipWith :: [Int] -> [Int] -> Int
flatMapAfterZipWith l1 l2 =
    foldl' (+) 0 $
    concat $
    map (\x -> map (x +) l2) $
    map (uncurry (+)) $
    zip l1 l1

zipWithAfterFlatMap :: [Int] -> [Int] -> Int
zipWithAfterFlatMap l1 l2 =
    foldl' (+) 0 $
    map (uncurry (+)) $
    zip l1 $
    concat $
    map (\x -> map (x +) l2) $
    l1

flatMapTake :: [Int] -> [Int] -> Int
flatMapTake l1 l2 =
    foldl' (+) 0 $
    take 20000000 $
    concat $
    map (\x -> map (x +) l2) $
    l1
