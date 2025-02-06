module T2Spec (tests) where

import HW4.T2 (ParseError (..), parseExpr)
import HW4.Types
import Test.HUnit

tests :: Test
tests = TestLabel "parseExpr" $ TestList [
        TestCase (assertEqual "#1" (show $ parseExpr "3.14 + 1.618 * 2") (show $ (Success (Op (Add (Val 3.14 ) (Op (Mul (Val 1.618) (Val 2.0))))) :: Except ParseError Expr))),
        TestCase (assertEqual "#2" (show $ parseExpr "2 * (1 + 3)") (show $ (Success (Op (Mul (Val 2.0) (Op (Add (Val 1.0) (Val 3.0))))) :: Except ParseError Expr))),
        TestCase (assertEqual "#3" (show $ parseExpr "24 + Hello") (show $ (Error (ErrorAtPos 3) :: Except ParseError Expr)))
    ]
