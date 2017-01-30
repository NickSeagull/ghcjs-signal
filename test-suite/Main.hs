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

        it "as a functor, satisfies the identity law" $ 
            property functorIdentity

        it "as a functor, satisfies the composition law" $ 
            property functorComposition

        it "as an applicative, satisifies the identity law" $ 
            property applicativeIdentity

        it "as an applicative, satisifies the homomorphism law" $ 
            property applicativeHomomorphism

        it "as an applicative, satisifies the composition law" $ 
            property applicativeComposition

        it "as an applicative, satisifies the interchange law" $ 
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