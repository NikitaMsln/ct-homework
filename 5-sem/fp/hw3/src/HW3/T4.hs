module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import Control.Monad (ap)
import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

stateToFun :: State s a -> Fun s (Annotated s a)
stateToFun (S f) = F f

funToState :: Fun s (Annotated s a) -> State s a
funToState (F f) = S f

mapState :: (a -> b) -> State s a -> State s b
mapState f = funToState . mapFun (mapAnnotated f) . stateToFun

wrapState :: a -> State s a
wrapState x = S (\t -> x :# t)

joinState :: State s (State s a) -> State s a
joinState st = S (\t -> let (x :# st0) = (runS st t) in runS x st0)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\t -> () :# (f t))

instance Functor (State s) where
  fmap = mapState

instance Monad (State s) where
  (>>=) m f = joinState (fmap f m)

instance Applicative (State s) where
  pure = wrapState
  (<*>) = ap

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
  (+) x y = Op (Add x y)
  (-) x y = Op (Sub x y)
  (*) x y= Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  (/) x y = Op (Div x y)
  fromRational x = Val (fromRational x)

evalElement :: a -> (a -> State [c] b) -> (b -> c) -> (b -> d) -> State [c] d
evalElement x evaluator modif ret = do
  evaluated <- evaluator x
  modifyState ((:) (modif evaluated))
  return (ret evaluated)

mapPair1 :: (a -> State s b) -> (a, a) -> State s (b, b)
mapPair1 f (x, y) = do
  x1 <- f x
  y1 <- f y
  return (x1, y1)

eval :: Expr -> State [Prim Double] Double
eval (Val v)        = pure v
eval (Op (Sgn v))   = evalElement v eval Sgn signum
eval (Op (Abs v))   = evalElement v eval Abs abs
eval (Op (Div x y)) = evalElement (x, y) (mapPair1 eval) (uncurry Div) (uncurry (/))
eval (Op (Mul x y)) = evalElement (x, y) (mapPair1 eval) (uncurry Mul) (uncurry (*))
eval (Op (Add x y)) = evalElement (x, y) (mapPair1 eval) (uncurry Add) (uncurry (+))
eval (Op (Sub x y)) = evalElement (x, y) (mapPair1 eval) (uncurry Sub) (uncurry (-))
