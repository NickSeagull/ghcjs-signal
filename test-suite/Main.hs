{-# LANGUAGE TypeOperators #-}

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.IO ()
import Signal

type A = Int
type B = Int
type C = Int
type (~>) a b = Fun a b

main :: IO ()
main = hspec $

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
functorIdentity x = runSignal $ 
    id <$> constant x 
    ~> ( `shouldBe` x )


functorComposition :: A ~> B
                   -> B ~> C
                   -> A
                   -> IO ()
functorComposition _F _G x = runSignal $
    f <$> g <$> constant x
    ~> ( `shouldBe` f (g x) )
  where 
    f = apply _F
    g = apply _G


applicativeIdentity :: A
                    -> IO ()
applicativeIdentity x = runSignal $
    pure id <*> pure x
    ~> ( `shouldBe` x )


applicativeHomomorphism :: A ~> B
                        -> A
                        -> IO ()
applicativeHomomorphism _F x = runSignal $
    pure f <*> pure x
    ~> ( `shouldBe` f x )
  where f = apply _F


applicativeComposition :: B ~> C
                       -> A ~> B
                       -> A
                       -> IO ()
applicativeComposition _F _G x = runSignal $
    pure (.) <*> apf <*> apg <*> apx
    ~> ( `shouldBe` (f . g) x  )
  where
    f = apply _F
    g = apply _G
    apf = pure f
    apg = pure g
    apx = pure x


applicativeInterchange :: A
                       -> A ~> B
                       -> IO ()
applicativeInterchange y _U = runSignal $
    pure (\x -> x $ y) <*> apu
    ~> ( `shouldBe` u y )
  where
    u = apply _U
    apu = pure u