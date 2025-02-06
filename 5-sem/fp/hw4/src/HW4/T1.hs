module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad (ap)
import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g)= ES (fmap (fmap f) . g)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES (\t -> Success (x :# t))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES (foldExcept id Error . fmap (\((ES g) :# y) -> g y) . f)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\t -> Success (() :# f t))

throwExceptState :: e -> ExceptState e s a
throwExceptState x = ES (Error . const x)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Monad (ExceptState e s) where
  (>>=) m f = joinExceptState (fmap f m)

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

data EvaluationError = DivideByZero
  deriving Show

evalElement :: a -> (a -> ExceptState e [c] b) -> (b -> c) -> (b -> Except e d) -> ExceptState e [c] d
evalElement x evaluator modif ret = do
  evaluated <- evaluator x
  modifyExceptState ((:) (modif evaluated))
  foldExcept return throwExceptState (ret evaluated)

mapPair1 :: (a -> ExceptState e s b) -> (a, a) -> ExceptState e s (b, b)
mapPair1 f (x, y) = do
  x1 <- f x
  y1 <- f y
  return (x1, y1)

divExceptEval :: (Double, Double) -> Except EvaluationError Double
divExceptEval (_, 0) = Error DivideByZero
divExceptEval (a, b) = Success (a / b)

binEval :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
binEval x y f g = evalElement (x, y) (mapPair1 eval) (uncurry f) (Success . uncurry g)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val v)        = pure v
eval (Op (Sgn v))   = evalElement v eval Sgn $ Success .signum
eval (Op (Abs v))   = evalElement v eval Abs $ Success . abs
eval (Op (Mul x y)) = binEval x y Mul (*)
eval (Op (Add x y)) = binEval x y Add (+)
eval (Op (Sub x y)) = binEval x y Sub (-)
eval (Op (Div x y)) = evalElement (x, y) (mapPair1 eval) (uncurry Div) (divExceptEval)
