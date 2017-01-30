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

        it "is a functor, so it preserves identity" $ do
            property functorIdentity

        it "is a functor, so it composes" $ do
            property functorComposition

        it "is an applicative, satisifies the identity law" $ do
            property applicativeIdentity

        it "is an applicative, satisifies the homomorphism law" $ do
            property applicativeHomomorphism

        it "is an applicative, satisifies the composition law" $ do
            property applicativeComposition


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