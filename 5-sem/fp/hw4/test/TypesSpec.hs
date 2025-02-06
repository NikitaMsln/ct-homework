{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module TypesSpec (tests, ExceptEqual(..), AnnotatedEq(..)) where

import HW4.Types (Annotated (..), Except (..), foldExcept)
import InstanceSpec
import Test.HUnit

exceptCheck :: (a -> Bool) -> e -> a -> Except e a
exceptCheck f x y = case f y of
    True  -> Success y
    False -> Error x

funcs :: [Int -> Int]
funcs = [(+) 1, (+) 234, flip mod 43, (*) 13, (-) 1]

newtype ExceptEqual a = EE (Except String a)
    deriving newtype (Show, Functor)

instance Eq a => Eq (ExceptEqual a) where
    (==) (EE (Error x)) (EE (Error y))     = x == y
    (==) (EE (Success x)) (EE (Success y)) = x == y
    (==) _ _                               = False

testExcept :: Test
testExcept = TestList [
        testFunctor funcs funcs $ fmap (EE . exceptCheck ((/=) 0 . flip mod 3) "E") ([-1000..1000] :: [Int]),
        TestCase (assertEqual "foldExcept"
            (fmap (foldExcept (\t -> 'S' : show t) (const "E") . exceptCheck ((/=) 0 . flip mod 3) "E") ([-1000..1000] :: [Int]))
            (fmap (\x -> if (mod x 3 /= 0) then 'S' : show x else "E") ([-1000..1000] :: [Int]))
        )
    ]

newtype AnnotatedEq a = AE (Annotated Int a)
    deriving newtype (Show, Functor)

instance Eq a => Eq (AnnotatedEq a) where
    (==) (AE (x :# y)) (AE (z :# w)) = x == z && y == w

testAnnotated :: Test
testAnnotated = testFunctor funcs funcs $ fmap (AE . \k -> (k :# (k - 1))) ([-1000..1000] :: [Int])

tests :: Test
tests = TestList [
        TestLabel "Except e" testExcept,
        TestLabel "Annotated e" testAnnotated
    ]
