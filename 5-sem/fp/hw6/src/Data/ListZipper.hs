module Data.ListZipper
  ( ListZipper (..),
    toLeft,
    toRight,
    exchangeCurr,
    buildByFun,
    cutListZipper
  ) where

import Control.Comonad (Comonad (..))

data Infin a = Inf a (Infin a)

buildInfByFunImpl :: (Integer -> a) -> Integer -> Infin a
buildInfByFunImpl f i = Inf (f i) $ buildInfByFunImpl f (i + 1)

buildInfByFun :: (Integer -> a) -> Infin a
buildInfByFun f = buildInfByFunImpl f 0

takeInf :: Integer -> Infin a -> [a]
takeInf i (Inf x xs) | i <= 0 = []
                     | otherwise = x : takeInf (i - 1) xs

instance Functor Infin where
  fmap f (Inf a as) = Inf (f a) (fmap f as)

data ListZipper a = LZ (Infin a) a (Infin a)

buildByFun :: (Integer -> a) -> ListZipper a
buildByFun f = LZ (buildInfByFun $ \i -> f (-i - 1)) (f 0) (buildInfByFun $ \i -> f (i + 1))

toLeft :: ListZipper a -> ListZipper a
toLeft (LZ (Inf x xs) y ys) = LZ xs x (Inf y ys)

toRight :: ListZipper a -> ListZipper a
toRight (LZ ys y (Inf x xs)) = LZ (Inf y ys) x xs

instance Functor ListZipper where
  fmap f (LZ x y z) = LZ (fmap f x) (f y) (fmap f z)

extendLeft :: (ListZipper a -> b) -> ListZipper a -> Infin b
extendLeft f l = Inf (f $ toLeft l) $ extendLeft f (toLeft l)

extendRight :: (ListZipper a -> b) -> ListZipper a -> Infin b
extendRight f l = Inf (f $ toRight l) $ extendRight f (toRight l)

exchangeCurr :: a -> ListZipper a -> ListZipper a
exchangeCurr x (LZ l _ r) = LZ l x r

cutListZipper :: Integer -> ListZipper a -> [a]
cutListZipper r (LZ x y z) = (reverse $ takeInf r x) ++ (y : takeInf r z)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  extend f lz = LZ (extendLeft f lz) (f lz) (extendRight f lz)
