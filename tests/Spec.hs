import Test.Hspec
import StringCalculator
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "StringCalculator" $ do
        it "should return 0 for an empty string" $ do
            calculate "" `shouldBe` 0

        it "should return 1 for a string '1'" $ do
            calculate "1" `shouldBe` 1

        it "should return 2 for a string '2'" $ do
            calculate "2" `shouldBe` 2

        it "should return 3 for a string '1,2'" $ do
            calculate "1,2" `shouldBe` 3

        it "should return 6 for a string '1\n2,3'" $ do
            calculate "1\n2,3" `shouldBe` 6

        it "should return 3 for a string of '//;\n1;2'" $ do
            calculate "//;\n1;2" `shouldBe` 3

        it "should throw an exception if a negative number is given" $ do
            evaluate (calculate "1,-2") `shouldThrow` errorCall "negatives not allowed"

