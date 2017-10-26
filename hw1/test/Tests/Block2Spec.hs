module Tests.Block2Spec where

import           Test.Hspec

import           Block2.Task1
import           Block2.Task2
import           Block2.Task3
import           Block2.Task4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Task1" $ do
    remove 3 [1..5] `shouldBe` (Just 4, [1,2,3,5])
    remove 10 [1..5] `shouldBe` (Nothing, [1..5])

  it "Task2" $ collectEvery 3 [1 .. 8] `shouldBe` ([1, 2, 4, 5, 7, 8], [3, 6])

  it "Task3" $ stringSum "100\n\t-3" `shouldBe` 97

  it "Task4" $ mergeSort [2, 1, 0, 3, 10, 5] `shouldBe` [0, 1, 2, 3, 5, 10]
