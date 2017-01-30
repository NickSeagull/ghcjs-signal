{-# LANGUAGE TypeOperators #-}

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.IO ()
import Data.Semigroup
import Data.Maybe
import Signal
import SignalTester

type A = Int
type B = Int
type C = Int
type (~>) a b = Fun a b

main :: IO ()
main = hspec $ do

    describe "The Signal tester" $ 
        it "can check if a Signal contains the values or not" $
            constant "Foo" `shouldYield` ["Foo"] 

    describe "A Signal" $ do

        it "can contain an IO action, and is able to run it after" $
            runSignal $ constant (return ())

        it "is a functor, it satisfies the identity law" $ 
            property functorIdentity

        it "is a functor, it satisfies the composition law" $ 
            property functorComposition

        it "is an applicative, it satisifies the identity law" $ 
            property applicativeIdentity

        it "is an applicative, it satisifies the homomorphism law" $ 
            property applicativeHomomorphism

        it "is an applicative, it satisifies the composition law" $ 
            property applicativeComposition

        it "is an applicative, it satisifies the interchange law" $ 
            property applicativeInterchange

        it "is able to merge with another signal, yielding in order" $
            property semigroupMerge
    
        it "is able to merge with multiple signals, yielding in order" $
            property semigroupMergeMany

        it "is able to map a function over each value that will be yielded" $
            ((* 2) <$> tick 1 1 [1,2,3]) `shouldYield` [2,4,6]



functorIdentity :: A
                -> IO ()
functorIdentity x = 
    (id <$> constant x)
    `shouldYield` [x]


functorComposition :: A ~> B
                   -> B ~> C
                   -> A
                   -> IO ()
functorComposition _F _G x = 
    (f <$> g <$> constant x)
    `shouldYield` [f (g x)]
  where 
    f = apply _F
    g = apply _G


applicativeIdentity :: A
                    -> IO ()
applicativeIdentity x =
    (pure id <*> pure x)
    `shouldYield` [x]


applicativeHomomorphism :: A ~> B
                        -> A
                        -> IO ()
applicativeHomomorphism _F x =
    (pure f <*> pure x)
    `shouldYield` [f x]
  where f = apply _F


applicativeComposition :: B ~> C
                       -> A ~> B
                       -> A
                       -> IO ()
applicativeComposition _F _G x =
    (pure (.) <*> apf <*> apg <*> apx)
    `shouldYield` [(f . g) x]
  where
    f = apply _F
    g = apply _G
    apf = pure f
    apg = pure g
    apx = pure x


applicativeInterchange :: A
                       -> A ~> B
                       -> IO ()
applicativeInterchange y _U = 
    (pure ($ y) <*> apu)
    `shouldYield` [u y]
  where
    u = apply _U
    apu = pure u


semigroupMerge :: A
               -> A
               -> IO ()
semigroupMerge x y =
    (constant x <> constant y)
    `shouldYield` [x]


semigroupMergeMany :: A
                   -> [A]
                   -> IO ()
semigroupMergeMany x xs =
    fromMaybe (constant 1337) (mergeMany testSignals)
    `shouldYield` [x]
  where
    testSignals = constant <$> (x:xs)

