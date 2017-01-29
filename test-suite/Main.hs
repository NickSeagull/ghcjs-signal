import Test.Hspec
import Signal

main :: IO ()
main = hspec $

    describe "A Signal" $ do

        it "can contain an IO action, able to run it after" $
            runSignal $ constant pass

        it "is a functor, so it can be mapped over" $ do
            let sig               = constant (2 :: Int)
            let sig'              = fmap (+1) sig
            let expectation value = value `shouldBe` (3 :: Int)
            let result            = expectation <$> sig'
            runSignal result

pass :: IO ()
pass = putStr ""