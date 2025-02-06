module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) other = x :+ other
  (<>) (h :+ t) other = h :+ (t <> other)

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This x) (This y)     = This (x <> y)
  (<>) (This x) (That y)     = Both x y
  (<>) (That x) (This y)     = Both y x
  (<>) (That x) (That y)     = That (x <> y)
  (<>) (Both x y) (This z)   = Both (x <> z) y
  (<>) (Both x y) (That z)   = Both x (y <> z)
  (<>) (This x) (Both y z)   = Both (x <> y) z
  (<>) (That x) (Both y z)   = Both y (x <> z)
  (<>) (Both x y) (Both z w) = Both (x <> z) (y <> w)

newtype DotString = DS { getString :: String }
  deriving Show

instance Semigroup DotString where
  (<>) (DS []) (DS y) = DS y
  (<>) (DS x) (DS []) = DS x
  (<>) (DS x) (DS y)  = DS $ x ++ '.' : y

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F { getFun :: a -> a}

instance Semigroup (Fun a) where
  (<>) (F f) (F g) = F $ f . g

instance Monoid (Fun a) where
  mempty = F id
