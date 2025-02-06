module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..), tail)
import Prelude hiding (tail)

-- You may add necessary constraints here
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn x = foldr (\y (h :| acc) -> if (x == y) then [] :| (h : acc) else (y : h) :| acc) ([] :| [])

joinWith :: a -> NonEmpty [a] -> [a]
joinWith x l = tail . foldr1 (<>) . fmap ((:|) x) $ l
