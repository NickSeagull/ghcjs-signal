{-# LANGUAGE TypeOperators #-}

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.IO ()
import Signal
import SignalTester

type A = Int
type B = Int
type C = Int
type (~>) a b = Fun a b

main :: IO ()
main = hspec $ do

    describe "The Signal tester" $ do
        it "can check if a Signal contains the values or not" $
            constant "Foo" `shouldContainValues` ["Foo"] 

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

    

functorIdentity :: A
                -> IO ()
functorIdentity x = 
    id <$> constant x 
    `shouldContainValues` [x]


functorComposition :: A ~> B
                   -> B ~> C
                   -> A
                   -> IO ()
functorComposition _F _G x = 
    f <$> g <$> constant x
    `shouldContainValues` [f (g x)]
  where 
    f = apply _F
    g = apply _G


applicativeIdentity :: A
                    -> IO ()
applicativeIdentity x =
    pure id <*> pure x
    `shouldContainValues` [x]


applicativeHomomorphism :: A ~> B
                        -> A
                        -> IO ()
applicativeHomomorphism _F x =
    pure f <*> pure x
    `shouldContainValues` [f x]
  where f = apply _F


applicativeComposition :: B ~> C
                       -> A ~> B
                       -> A
                       -> IO ()
applicativeComposition _F _G x =
    pure (.) <*> apf <*> apg <*> apx
    `shouldContainValues` [(f . g) x]
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
    pure ($ y) <*> apu
    `shouldContainValues` [u y]
  where
    u = apply _U
    apu = pure u