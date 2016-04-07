module HW1.LuhnSpec (main, spec) where

import           HW1.Luhn
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "toDigitsRev" $ do
        it "converts positive Integers to a reversed list of its digits" $
            toDigitsRev 1234 `shouldBe` [4,3,2,1]

        it "returns an empty list if the Integer is not positive" $ do
            toDigitsRev 0 `shouldBe` []
            toDigitsRev (-17) `shouldBe` []


    describe "toDigits" $ do
        it "converts positive Integers to a list of its digits" $
            toDigits 1234 `shouldBe` [1,2,3,4]

        it "returns an empty list if the Integer is not positive" $ do
            toDigits 0 `shouldBe` []
            toDigits (-17) `shouldBe` []


    describe "doubleEveryOther" $ do
        it "doubles every other number beginning with the second-to-last" $ do
            doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
            doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

        it "returns the same list if there is only one element" $
            doubleEveryOther [42] `shouldBe` [42]

        it "returns an empty list if the input list is empty" $
            doubleEveryOther [] `shouldBe` []


    describe "sumDigits" $ do
        it "sums a list of one-digit and two-digit Integers" $
            sumDigits [16,7,12,5] `shouldBe` 22

        it "returns 0 if the input list is empty" $
            sumDigits [] `shouldBe` 0


    describe "validate" $
        it "indicates whether an Integer could be a valid credit card number" $ do
            validate 4012888888881881 `shouldBe` True
            validate 4012888888881882 `shouldBe` False
