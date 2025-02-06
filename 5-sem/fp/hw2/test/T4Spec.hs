module T4Spec (tests) where

import Data.Semigroup (Sum (..))
import HW2.T4
import Test.HUnit

toListPlus :: a -> [a] -> ListPlus a
toListPlus x (h : t) = x :+ (toListPlus h t)
toListPlus x []      = Last x

test1 :: Test
test1 = TestCase (assertEqual "assoc"
        (show $ toListPlus (1 :: Int) [2..34] <> (toListPlus 5 [6..9] <> toListPlus 0 [123..132]))
        (show $ (toListPlus (1 :: Int) [2..34] <> toListPlus 5 [6..9]) <> toListPlus 0 [123..132])
    )

test2 :: Test
test2 = TestCase (assertEqual "assoc"
        (show $ This (Sum (1 :: Int)) <> (That "b" <> Both (Sum 6) "5"))
        (show $ (This (Sum (1 :: Int)) <> That "b") <> Both (Sum 6) "5")
    )

test3 :: Test
test3 = TestCase (assertEqual "assoc" (getString $ DS "adadwa" <> (DS "afage" <> DS "aeeageg")) (getString $ (DS "adadwa" <> DS "afage") <> DS "aeeageg"))

test4 :: Test
test4 = TestCase (assertEqual "left id" (getString $ DS "1234") (getString $ mempty <> DS "1234"))

test5 :: Test
test5 = TestCase (assertEqual "right id" (getString $ DS "1234") (getString $ DS "1234" <> mempty))

data FuncEq a = FE [a] (Fun a)

instance Eq a => Eq (FuncEq a) where
    (==) (FE sample (F f)) (FE _ (F g)) = (map f sample) == (map g sample)

instance Show a => Show (FuncEq a) where
    show (FE l _) = "sample: " ++ (show l)

test6 :: Test
test6 = TestCase (assertEqual "assoc" (FE [-1000..1000] ((F ((+) 1 :: Int -> Int) <> F ((*) 2)) <> F ((-) 0))) (FE [-1000..1000] (F ((+) 1) <> (F ((*) 2) <> F ((-) 0)))))

test7 :: Test
test7 = TestCase (assertEqual "left id" (FE [-1000..1000] (F ((+) 1 :: Int -> Int))) (FE [-1000..1000] (mempty <> F ((+) 1))))

test8 :: Test
test8 = TestCase (assertEqual "right id" (FE [-1000..1000] (F ((+) 1 :: Int -> Int))) (FE [-1000..1000] (F ((+) 1) <> mempty)))

tests :: Test
tests = TestList [
        TestLabel "ListPlus" test1,
        TestLabel "Inclusive" test2,
        TestLabel "DotString" test3,
        TestLabel "DotString" test4,
        TestLabel "DotString" test5,
        TestLabel "Fun a" test6,
        TestLabel "Fun a" test7,
        TestLabel "Fun a" test8
    ]
