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


functorIdentity :: A
                -> IO ()
functorIdentity x = runSignal $ 
    constant x 
    ~> id
    ~> ( `shouldBe` x )


functorComposition :: A ~> B
                   -> B ~> C
                   -> A
                   -> IO ()
functorComposition f g x = runSignal $
    constant x
    ~> (apply g)
    ~> (apply f)
    ~> ( `shouldBe` f_after_g x )
  where f_after_g = (apply f) . (apply g)


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

