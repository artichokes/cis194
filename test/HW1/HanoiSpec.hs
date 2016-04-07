module HW1.HanoiSpec (main, spec) where

import           HW1.Hanoi
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "hanoi" $ do
        it "returns steps to move 2 pegs from A to C" $
            hanoi 2 "A" "B" "C" `shouldBe` [("A", "B"), ("A", "C"), ("B", "C")]

        it "requires 2^n - 1 steps to move n pegs from A to C" $
            length (hanoi 8 "A" "B" "C") `shouldBe` 255


    describe "hanoi4" $ do
        it "returns steps to move 2 pegs from A to C" $
            hanoi4 3 "A" "B" "C" "D" `shouldBe` [("A","B"),("A","C"),("A","D"),("C","D"),("B","D")]

        it "requires 129 steps to move 15 pegs from A to C" $
            length (hanoi4 15 "A" "B" "C" "D") `shouldBe` 129
