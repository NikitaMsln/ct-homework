module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import Data.Char (isSpace)
import HW0.T1 (distrib)

a :: (Either String a, Either String b)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

a_whnf :: (Either String a, Either String b)
a_whnf = (x, x)
  where
    x = Left ("AB" ++ "CD" ++ "EF")

b :: [Bool]
b = map isSpace "Hello, World"

b_whnf :: [Bool]
b_whnf = (isSpace 'H') : (map isSpace "ello, World")

c :: String
c = if (1 :: Int) > 0 || error "X" then "Y" else "Z"

c_whnf :: String
c_whnf = "Y"
