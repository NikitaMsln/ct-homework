{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left x)       = (Left x, Left x)
distrib (Right (y, z)) = (Right y, Right z)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso x y) = Iso y x

runIso :: (a <-> b) -> (a -> b)
runIso (Iso x _) = x

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(x, (y, z)) -> ((x, y), z)) (\((x, y), z) -> (x, (y, z)))

assocEitherLeft :: Either a (Either b c) -> Either (Either a b) c
assocEitherLeft (Left x)          = Left (Left x)
assocEitherLeft (Right (Left y))  = Left (Right y)
assocEitherLeft (Right (Right z)) = Right z

assocEitherRight :: Either (Either a b) c -> Either a (Either b c)
assocEitherRight (Left (Left x))  = Left x
assocEitherRight (Left (Right y)) = Right (Left y)
assocEitherRight (Right z)        = Right (Right z)

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso assocEitherLeft assocEitherRight
