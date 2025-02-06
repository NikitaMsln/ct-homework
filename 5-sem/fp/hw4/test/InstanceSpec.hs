module InstanceSpec (testFunctor, testApplicative, testMonad) where

import Test.HUnit

testFunctor :: (Functor f, Show (f a), Show (f c), Eq (f a), Eq (f c)) => [a -> b] -> [b -> c] -> [f a] -> Test
testFunctor fl gl testSet = TestLabel "Functor laws" $ TestList [
        (TestCase $ assertEqual "id"
            (fmap (fmap id) testSet)
            testSet
        ),
        (TestCase $ assertEqual "composition"
            (flip fmap gl $ \g -> flip fmap fl $ \f1 -> flip fmap testSet $ \t -> fmap (g . f1) t)
            (flip fmap gl $ \g -> flip fmap fl $ \f1 -> flip fmap testSet $ \t -> fmap g $ fmap f1 t)
        )
    ]

pureByExample :: Applicative f => [f a] -> b -> f b
pureByExample _ x = pure x

testApplicative :: (Applicative f, Show (f a), Show (f b), Show (f c), Eq (f a), Eq (f b), Eq (f c)) => [a -> b] -> [a] -> [f (a -> b)] -> [f (b -> c)] -> [f a] -> Test
testApplicative pureF pureA ff fg fa = TestLabel "Applicative laws" $ TestList [
        (TestCase $ assertEqual "id"
            (fmap (pure id <*>) fa)
            fa
        ),
        (TestCase $ assertEqual "composition"
            (flip fmap fg $ \g -> flip fmap ff $ \f1 -> flip fmap fa $ \x -> pure (.) <*> g <*> f1 <*> x)
            (flip fmap fg $ \g -> flip fmap ff $ \f1 -> flip fmap fa $ \x -> g <*> (f1 <*> x))
        ),
        (TestCase $ assertEqual "homomorphism"
            (flip fmap pureA $ \x -> flip fmap pureF $ \f1 -> pureByExample ff $ f1 x)
            (flip fmap pureA $ \x -> flip fmap pureF $ \f1 -> (pure f1) <*> (pure x))
        ),
        (TestCase $ assertEqual "interchange"
            (flip fmap ff $ \f1 -> flip fmap pureA $ \x -> f1 <*> pure x)
            (flip fmap ff $ \f1 -> flip fmap pureA $ \x -> pure ($ x) <*> f1)
        )
    ]

testMonad :: (Monad m, Show (m a), Show (m b), Eq (m a), Eq (m b)) => [a] -> [m a] -> [a -> m b] -> [b -> m a] -> Test
testMonad pa ma mf mg = TestLabel "Monad laws" $ TestList [
        (TestCase $ assertEqual "left id"
            (flip fmap mf $ \f -> flip fmap pa $ \x -> return x >>= f)
            (flip fmap mf $ \f -> flip fmap pa $ \x -> f x)
        ),
        (TestCase $ assertEqual "right id"
            (fmap (>>= return) ma)
            ma
        ),
        (TestCase $ assertEqual "associativity"
            (flip fmap mg $ \g -> flip fmap mf $ \f -> flip fmap ma $ \x -> x >>= (\y -> f y >>= g))
            (flip fmap mg $ \g -> flip fmap mf $ \f -> flip fmap ma $ \x -> (x >>= f) >>= g)
        )
    ]
