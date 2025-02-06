module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr (curry fst) mempty . mconcat

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap id . map (either (\x -> (x, mempty)) (\y -> (mempty, y)))
