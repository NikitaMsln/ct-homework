module Data.Grid
  ( Grid (..),
    toLeftGrid,
    toRightGrid,
    toTopGrid,
    toBottomGrid,
    exchangeCurrGrid,
    buildGridByFun,
    cutGrid
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper (ListZipper (..), buildByFun, cutListZipper, exchangeCurr, toLeft, toRight)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

toLeftGrid :: Grid a -> Grid a
toLeftGrid = Grid . fmap toLeft . unGrid

toRightGrid :: Grid a -> Grid a
toRightGrid = Grid . fmap toRight . unGrid

toTopGrid :: Grid a -> Grid a
toTopGrid = Grid . toLeft . unGrid

toBottomGrid :: Grid a -> Grid a
toBottomGrid = Grid . toRight . unGrid

exchangeCurrGrid :: a -> Grid a -> Grid a
exchangeCurrGrid x g = Grid . exchangeCurr (exchangeCurr x . extract $ unGrid g) $ unGrid g

buildGridByFun :: (Integer -> Integer -> a) -> Grid a
buildGridByFun f = Grid $ buildByFun (\i -> buildByFun (f i))

cutGrid :: Integer -> Grid a -> [[a]]
cutGrid r = cutListZipper r . fmap (cutListZipper r) . unGrid

extendToListZipper :: (a -> a) -> (a -> a) -> a -> ListZipper a
extendToListZipper l r x = buildByFun $ \i -> if i < 0 then iterate l x !! (fromInteger $ negate i) else iterate r x !! fromInteger i

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract = extract . extract . unGrid

  duplicate = Grid . fmap (extendToListZipper toLeftGrid toRightGrid) . fmap Grid . duplicate . unGrid
