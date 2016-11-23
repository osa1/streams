{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}

module Streams.Unfold where

--------------------------------------------------------------------------------

import Data.Bifunctor (first, second)
import Data.List (unfoldr)

--------------------------------------------------------------------------------

data Stream a = forall e . Stream e (e -> Maybe (a, e))

--------------------------------------------------------------------------------

fromList :: [a] -> Stream a
fromList lst = Stream lst step
  where
    step []      = Nothing
    step (h : t) = Just (h, t)

fold :: (b -> a -> b) -> b -> Stream a -> b
fold f !b (Stream e step) =
    maybe b (\(a, e') -> fold f (f b a) (Stream e' step)) (step e)

foldM :: Monad m => (b -> a -> m b) -> b -> Stream a -> m b
foldM f b (Stream e step) =
    case step e of
      Nothing      -> return b
      Just (a, e') -> do
        b' <- f b a
        foldM f b' (Stream e' step)

map :: (a -> b) -> Stream a -> Stream b
map f (Stream e0 step) = Stream e0 (fmap (first f)  . step)

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Stream e0 step) = Stream e0 step'
  where
    step' e = do
      (a, e') <- step e
      if p a
        then return (a, e')
        else step' e'

snoc :: Stream a -> a -> Stream a
snoc (Stream e0 step) a = Stream (False, e0) step'
  where
    step' (True,  _) = Nothing
    step' (False, e) = Just (maybe (a, (True, undefined)) (second (False,)) (step e))

cons :: a -> Stream a -> Stream a
cons a (Stream e0 step) = Stream (False, e0) step'
  where
    step' (True,  e) = fmap (second (True,)) (step e)
    step' (False, e) = Just (a, (True, e))

zip :: Stream a -> Stream b -> Stream (a, b)
zip (Stream e1_0 step1) (Stream e2_0 step2) = Stream (e1_0, e2_0) step'
  where
    step' (e1, e2) = do
      (a, e1') <- step1 e1
      (b, e2') <- step2 e2
      return ((a, b), (e1', e2'))

append :: Stream a -> Stream a -> Stream a
append (Stream e1_0 step1) (Stream e2_0 step2) = Stream (Left e1_0) step'
  where
    step' (Left e1) =
      case step1 e1 of
        Nothing       -> step' (Right e2_0)
        Just (a, e1') -> Just (a, Left e1')

    step' (Right e2) =
      fmap (second Right) (step2 e2)

flatten :: Stream (Stream a) -> Stream a
flatten (Stream e_outer0 step_outer) = Stream (Left e_outer0) step'
  where
    step' (Left e_outer) = do
      (stream_inner, e_outer') <- step_outer e_outer
      step' (Right (e_outer', stream_inner))

    step' (Right (e_outer, Stream e_inner step_inner)) =
      case step_inner e_inner of
        Nothing            -> step' (Left e_outer)
        Just (a, e_inner') -> Just (a, Right (e_outer, Stream e_inner' step_inner))

take :: Int -> Stream a -> Stream a
take n0 (Stream e0 step) = Stream (e0, n0) step'
  where
    step' (_, 0) = Nothing
    step' (e, n) =
      case step e of
        Nothing      -> Nothing
        Just (a, e') -> Just (a, (e', n - 1))

drop :: Int -> Stream a -> Stream a
drop n0 (Stream e0 step) = Stream (iter n0 e0) step'
  where
    iter 0 e = Just e
    iter n e = case step e of
                 Nothing      -> Nothing
                 Just (_, e') -> iter (n - 1) e'

    step' Nothing  = Nothing
    step' (Just e) = case step e of
                       Nothing      -> Nothing
                       Just (a, e') -> Just (a, (Just e'))

toList :: Stream a -> [a]
toList (Stream e0 step) = unfoldr step e0
