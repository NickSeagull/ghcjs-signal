import Test.Hspec
import Signal

main :: IO ()
main = hspec $ do

    describe "A Signal" $ do

        it "can contain an IO action, able to run it after" $ do
            runSignal . constant $ pass

pass :: IO ()
pass = putStr ""