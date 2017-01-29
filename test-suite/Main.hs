import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.IO ()
import Signal

main :: IO ()
main = hspec $

    describe "A Signal" $ do

        it "can contain an IO action, and is able to run it after" $
            runSignal $ constant (return ())

        it "is a functor, so it can be mapped over" $ do
            property functorProperty

functorProperty :: Int -> Int -> IO ()
functorProperty x y = runSignal $ 
    constant x 
    ~> (+ y)
    ~> (`shouldBe` x + y)