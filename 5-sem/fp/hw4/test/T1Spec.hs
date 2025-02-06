{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module T1Spec (tests) where

import Control.Monad (join)
import HW4.T1 (EvaluationError (..), ExceptState (..), eval)
import HW4.Types (Annotated (..), Except (..), Prim (..))
import InstanceSpec
import Test.HUnit
import TypesSpec (AnnotatedEq (..), ExceptEqual (..))

testSample :: [Int]
testSample = [-100..100]

funcs :: [Int -> Int]
funcs = [(+) 1, (+) 234, flip mod 43, (*) 13, (-) 1, (*) 0]

newtype ESEqual a = ESE (ExceptState String Int a)
    deriving newtype (Functor, Monad, Applicative)

instance (Eq a) => Eq (ESEqual a) where
    (==) (ESE (ES f)) (ESE (ES g)) = (fmap (EE . fmap AE . f) testSample) == (fmap (EE . fmap AE . g) testSample)

instance (Show a) => Show (ESEqual a) where
    show (ESE (ES f)) = "ESE: " ++ (show $ fmap (\x -> (x, EE . fmap AE $ f x)) [-3..3])

aESEq :: [Int -> Int] -> [ESEqual Int]
aESEq = fmap (\f -> ESE . ES $ \t -> if (f t == 0) then Error "Z" else (Success $ f t :# (f t - 12)))

fESEq :: [ESEqual (Int -> Int)]
fESEq = fmap (\f -> ESE . ES $ \t -> if (f t t == 0) then Error "Z" else (Success $ f t :# f t t)) [(+), (*), (-), \x y -> x * x + y * y]

afESEq :: [Int -> ESEqual Int]
afESEq = join $ flip fmap [-10..10] $ \x -> flip fmap [(+), (*), (-), \p q -> p * p + q * q] $ \y -> \p -> ESE $ ES (\t -> Success (y (y t x) p :# (y p t)))

testESFunctor :: Test
testESFunctor = testFunctor funcs funcs $ aESEq funcs

testESApplicative :: Test
testESApplicative = testApplicative funcs [-20..20] fESEq fESEq $ aESEq funcs

testESMonad :: Test
testESMonad = testMonad [-20..20] (aESEq funcs) afESEq afESEq

testEval :: Test
testEval = TestLabel "eval" $ TestList [
        TestCase (assertEqual "single"
            (show $ runES (eval 1) [])
            (show (Success (1 :# []) :: Except EvaluationError (Annotated [Prim Double] Double)))
        ),
        TestCase (assertEqual "simple error"
            (show $ runES (eval (1 / 0)) [])
            (show (Error DivideByZero :: Except EvaluationError (Annotated [Prim Double] Double)))
        ),
        TestCase (assertEqual "comp error"
            (show $ runES (eval (5 + 1 * 2 / (3 - 3))) [])
            (show (Error DivideByZero :: Except EvaluationError (Annotated [Prim Double] Double)))
        ),
        TestCase (assertEqual "comp error"
            (show $ runES (eval (2 + 3 * 5 - 7)) [])
            (show (Success (10 :# [Sub 17.0 7, Add 2 15, Mul 3 5]) :: Except EvaluationError (Annotated [Prim Double] Double)))
        )
    ]

tests :: Test
tests = TestLabel "ExceptState" $ TestList [
        testESFunctor,
        testESApplicative,
        testESMonad,
        testEval
    ]
