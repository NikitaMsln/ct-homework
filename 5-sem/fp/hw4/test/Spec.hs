import qualified T1Spec
import qualified T2Spec
import Test.HUnit
import qualified TypesSpec

tests :: Test
tests = TestList [TestLabel "Types tests" TypesSpec.tests, TestLabel "T1 tests" T1Spec.tests, TestLabel "T2 tests" T2Spec.tests]

main :: IO ()
main = runTestTTAndExit tests
