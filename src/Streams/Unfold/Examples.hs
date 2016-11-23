module Streams.Unfold.Examples where

--------------------------------------------------------------------------------

import Streams.Unfold

import Prelude hiding (filter, map, take, zip)

--------------------------------------------------------------------------------

square :: Num a => a -> a
square x = x * x

sum :: [Int] -> Int
sum l =
    fold (+) 0 (fromList l)

sumOfSquares :: [Int] -> Int
sumOfSquares l =
    fold (+) 0 . map square $ fromList l

sumOfSquaresEven :: [Int] -> Int
sumOfSquaresEven l =
    fold (+) 0 . map square . filter even $ fromList l

cart :: [Int] -> [Int] -> Int
cart l1 l2 =
    fold (+) 0 $
    flatten $
    map (\x -> map (x *) (fromList l2)) $
    fromList l1

maps :: [Int] -> Int
maps l =
    fold (+) 0 $
    map (7 *) $
    map (6 *) $
    map (5 *) $
    map (4 *) $
    map (3 *) $
    map (2 *) $
    map (1 *) $
    fromList l

filters :: [Int] -> Int
filters l =
    fold (+) 0 $
    filter (> 7) $
    filter (> 6) $
    filter (> 5) $
    filter (> 4) $
    filter (> 3) $
    filter (> 2) $
    filter (> 1) $
    fromList l

dotProduct :: [Int] -> [Int] -> Int
dotProduct l1 l2 =
    fold (+) 0 $
    map (uncurry (*)) $
    zip (fromList l1) (fromList l2)

flatMapAfterZipWith :: [Int] -> [Int] -> Int
flatMapAfterZipWith l1 l2 =
    fold (+) 0 $
    flatten $
    map (\x -> map (x +) (fromList l2)) $
    map (uncurry (+)) $
    zip (fromList l1) (fromList l1)

zipWithAfterFlatMap :: [Int] -> [Int] -> Int
zipWithAfterFlatMap l1 l2 =
    fold (+) 0 $
    map (uncurry (+)) $
    zip (fromList l1) $
    flatten $
    map (\x -> map (x +) (fromList l2)) $
    fromList l1

flatMapTake :: [Int] -> [Int] -> Int
flatMapTake l1 l2 =
    fold (+) 0 $
    take 20000000 $
    flatten $
    map (\x -> map (x +) (fromList l2)) $
    fromList l1
