import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Signal" $ do
        it "is true" $ do
            1 + 1 `shouldBe` (2 :: Int)
