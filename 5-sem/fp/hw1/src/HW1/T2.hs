module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus first Z          = first
nplus first (S second) = nplus (S first) second

nmult :: N -> N -> N
nmult _ Z              = Z
nmult first (S second) = nplus (nmult first second) first

nsub :: N -> N -> Maybe N
nsub (S first) (S second) = nsub first second
nsub first Z              = Just first
nsub Z (S _)              = Nothing

ncmp :: N -> N -> Ordering
ncmp first second = case (nsub first second) of
  Nothing    -> LT
  Just Z     -> EQ
  Just (S _) -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S . nFromNatural $ n - 1

nToNum :: Num a => N -> a
nToNum Z         = 0
nToNum (S predv) = (+) 1 $ nToNum predv

nEven :: N -> Bool
nEven Z         = True
nEven (S predv) = not $ nEven predv

nOdd :: N -> Bool
nOdd = not . nEven

ndivMod :: N -> N -> (N, N)
ndivMod first Z = (first, first)
ndivMod first second = case nsub first second of
  Nothing        -> (Z, first)
  Just Z         -> (S Z, Z)
  Just (S predv) -> (\(x, y) -> (S x, y)) $ ndivMod (S predv) second

ndiv :: N -> N -> N
ndiv = curry (fst . uncurry ndivMod)

nmod :: N -> N -> N
nmod = curry (snd . uncurry ndivMod)
