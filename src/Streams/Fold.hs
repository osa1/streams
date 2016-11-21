{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Streams.Fold where

--------------------------------------------------------------------------------

data Stream a = Stream (forall w . (Maybe (a, w) -> w) -> w)

--------------------------------------------------------------------------------

fromList :: [a] -> Stream a
fromList lst = Stream push
  where
    push step = iter lst (step Nothing)
      where
        iter []       f = f
        iter (x : xs) f = iter xs (step (Just (x, f)))

fold :: (b -> a -> b) -> b -> Stream a -> b
fold f b (Stream push) =
    push (\case Nothing     -> b
                Just (a, x) -> f x a)


map :: (a -> b) -> Stream a -> Stream b
map f (Stream push) = Stream push'
  where
    push' step =
      push (\case Nothing     -> step Nothing
                  Just (a, w) -> step (Just (f a, w)))

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Stream push) = Stream push'
  where
    push' step =
      push (\case Nothing       -> step Nothing
                  Just (a, w)
                    | p a       -> step (Just (a, w))
                    | otherwise -> w)

snoc :: Stream a -> a -> Stream a
snoc (Stream push) a = Stream push'
  where
    push' step = step (Just (a, push step))

cons :: a -> Stream a -> Stream a
cons a (Stream push) = Stream push'
  where
    push' step = push (\case Nothing  -> step (Just (a, step Nothing))
                             just     -> step just)

-- zip: Not possible

append :: Stream a -> Stream a -> Stream a
append (Stream push1) (Stream push2) = Stream push'
  where
    push' step = push2 (\case Nothing     -> push1 step
                              Just (a, w) -> step (Just (a, w)))

flatten :: Stream (Stream a) -> Stream a
flatten (Stream push) = Stream push'
  where
    push' step =
      push (\case Nothing -> step Nothing
                  Just (Stream push_inner, w) ->
                    push_inner (\case Nothing -> w
                                      Just (a, w') -> step (Just (a, w'))))

-- take: TODO
-- drop: TODO

toList :: Stream a -> [a]
toList = reverse . fold (flip (:)) []
