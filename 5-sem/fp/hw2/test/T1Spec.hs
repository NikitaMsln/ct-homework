module T1Spec (tests) where

import HW2.T1
import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "sorted invariant" [1 :: Integer, 2] (tfoldr (:) [] (Branch 2 Leaf 1 (Branch 1 Leaf 2 Leaf))))

test2 :: Test
test2 = TestCase (assertEqual "counter" (2 :: Integer) (tfoldr (\_ -> (+) 1) 0 (Branch 2 Leaf (1 :: Int) (Branch 1 Leaf 2 Leaf))))

tests :: Test
tests = TestList [TestLabel "tfoldr" test1, TestLabel "tfoldr" test2]
