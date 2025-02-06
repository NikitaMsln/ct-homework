module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some x, Some y) = Some (x, y)

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x y, P z w) = P (x, z) (y, w)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 y1 z1 w1, Q x2 y2 z2 w2) = Q (x1, x2) (y1, y2) (z1, z2) (w1, w2)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

-- You may add necessary constraints here
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# y, z :# w) = (x, z) :# (y <> w)

-- You may add necessary constraints here
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error x, _)           = Error x
distExcept (_, Error x)           = Error x
distExcept (Success x, Success y) = Success (x, y)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low x, Low y)       = Low (x, y)
distPrioritised (Low x, Medium y)    = Medium (x, y)
distPrioritised (Medium x, Low y)    = Medium (x, y)
distPrioritised (Medium x, Medium y) = Medium (x, y)
distPrioritised (High x, Medium y)   = High (x, y)
distPrioritised (Medium x, High y)   = High (x, y)
distPrioritised (High x, Low y)      = High (x, y)
distPrioritised (Low x, High y)      = High (x, y)
distPrioritised (High x, High y)     = High (x, y)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> y, z :> w) = (x, z) :> (distStream (y, w))

wrapStream :: a -> Stream a
wrapStream x = x :> (wrapStream x)

concatList :: List a -> List a -> List a
concatList Nil l      = l
concatList (x :. y) l = x :. (concatList y l)

distList :: (List a, List b) -> List (a, b)
distList (x :. y, l) = concatList (mapList (\t -> (x, t)) l) $ distList (y, l)
distList _           = Nil

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\t -> (f t, g t))

wrapFun :: a -> Fun i a
wrapFun x = F (\_ -> x)
