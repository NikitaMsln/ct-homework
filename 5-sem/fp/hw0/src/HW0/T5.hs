module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz = flip const

ns :: Nat a -> Nat a
ns x f t = f (x f t)

nplus :: Nat a -> Nat a -> Nat a
nplus x y f t = x f (y f t)

nmult :: Nat a -> Nat a -> Nat a
nmult x y f = x (y f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural a = ns (nFromNatural (a - 1))

nToNum :: Num a => Nat a -> a
nToNum x = x ((+) 1) 0
