module T3Spec (tests) where

import Data.Semigroup (Sum (..))
import HW2.T3
import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "test 1" "monoid" (mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"]))

test2 :: Test
test2 = TestCase (assertEqual "test 2" (Sum (42 :: Int)) (mcat [Nothing, Just (Sum 2), Nothing, Just (Sum 40)]))

test3 :: Test
test3 = TestCase (assertEqual "test 1" (Sum (8 :: Int), [1..5] :: [Int]) (epart [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]))

tests :: Test
tests = TestList [TestLabel "mcat" test1, TestLabel "mcat" test2, TestLabel "epart" test3]
