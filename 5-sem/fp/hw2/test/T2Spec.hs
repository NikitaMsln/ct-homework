module T2Spec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import HW2.T2
import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "splitting" ("path" :| ["to","file"]) (splitOn '/' "path/to/file"))

test2 :: Test
test2 = TestCase (assertEqual "empty" ([] :| []) (splitOn '-' ""))

test3 :: Test
test3 = TestCase (assertEqual "joining" "Data.List.NonEmpty" (joinWith '.' ("Data" :| "List" : "NonEmpty" : [])))

test4 :: Test
test4 = TestCase (assertEqual "single" "single" (joinWith '-' $ "single" :| []))

tests :: Test
tests = TestList [TestLabel "splitOn" test1, TestLabel "splitOn" test2, TestLabel "joinWith" test3, TestLabel "joinWith" test4]
