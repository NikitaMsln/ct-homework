module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

-- S
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f1 f2 x = f1 x (f2 x)

-- K
k :: a -> b -> a
k x _ = x

-- I
i :: a -> a
i = s k k

-- B
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

-- W
contract :: (a -> a -> b) -> (a -> b)
contract = s s (k i)

-- C
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k compose) s) (k k)
