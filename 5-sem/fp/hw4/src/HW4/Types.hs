-- | This module contains the types from hw3 that are also
-- needed for hw4.
module HW4.Types
  ( Annotated (..)
  , Except (..)
  , Expr (..)
  , Prim (..)
  , State (..)
  , foldExcept
  ) where

data Except e a = Error e | Success a
  deriving Show

instance Functor (Except e) where
  fmap f (Success x) = Success $ f x
  fmap _ (Error x)   = Error x

foldExcept :: (a -> b) -> (e -> b) -> Except e a -> b
foldExcept f _ (Success x) = f x
foldExcept _ f (Error x)   = f x

data Annotated e a = a :# e
  deriving Show

instance Functor (Annotated e) where
  fmap f (x :# y) = f x :# y

data State s a = S { runS :: s -> Annotated s a }

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)
