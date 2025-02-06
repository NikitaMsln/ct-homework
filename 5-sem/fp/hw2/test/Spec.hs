import qualified T1Spec
import qualified T2Spec
import qualified T3Spec
import qualified T4Spec
import Test.HUnit

tests :: Test
tests = TestList [TestLabel "T1 tests" T1Spec.tests, TestLabel "T2 tests" T2Spec.tests, TestLabel "T3 tests" T3Spec.tests, TestLabel "T4 tests" T4Spec.tests]

main :: IO ()
main = runTestTTAndExit tests
