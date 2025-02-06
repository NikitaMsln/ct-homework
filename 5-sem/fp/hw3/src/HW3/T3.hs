module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption None     = None
joinOption (Some x) = x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error x)   = Error x
joinExcept (Success x) = x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# y) :# z) = x :# (z <> y)

joinList :: List (List a) -> List a
joinList ((x :. y) :. z) = x :. (joinList (y :. z))
joinList (Nil :. z)      = joinList z
joinList _               = Nil

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\t -> let (F g) = f t in g t)
