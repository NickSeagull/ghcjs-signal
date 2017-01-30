import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.IO ()
import Signal

main :: IO ()
main = hspec $

    describe "A Signal" $ do

        it "can contain an IO action, and is able to run it after" $
            runSignal $ constant (return ())

        it "is a functor, so it preserves identity" $ do
            property functorIdentity

        it "is a functor, so it composes" $ do
            property functorComposition

functorIdentity :: Int -> IO ()
functorIdentity x = runSignal $ 
    constant x 
    ~> id
    ~> ( `shouldBe` x )

functorComposition :: (Fun Int Int)
                   -> (Fun Int Int)
                   -> Int
                   -> IO ()
functorComposition f g x = runSignal $
    constant x
    ~> (apply g)
    ~> (apply f)
    ~> ( `shouldBe` f_after_g x )
  where f_after_g = (apply f) . (apply g)