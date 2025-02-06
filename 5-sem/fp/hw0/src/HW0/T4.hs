module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\req t -> if (null t) then [] else (f (head t) : req (tail t)))

fib :: Natural -> Natural
fib = fix (\req a b n -> if (n <= 0) then a else req b (a + b) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\req n -> if (n <= 0) then 1 else n * (req (n - 1)))
