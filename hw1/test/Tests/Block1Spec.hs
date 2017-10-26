module Tests.Block1Spec where

import           Test.Hspec

import           Block1.Task1
import           Block1.Task2
import           Block1.Task3
import           Block1.Task4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Task1" $ do
    order3 (1 :: Int, 2, 3) `shouldBe` (1 :: Int, 2, 3)
    order3 ('c', 'b', 'a') `shouldBe` ('a', 'b', 'c')

  it "Task2" $ do
    highestBit 15 `shouldBe` (8, 3)
    highestBit 16 `shouldBe` (16, 4)
    highestBit 17 `shouldBe` (16, 4)
    highestBit 1 `shouldBe` (1, 0)

  it "Task3" $ smartReplicate [1 :: Int, 2, 3] `shouldBe` [1 :: Int, 2, 2, 3, 3, 3]

  it "Task4" $ contains 3 [[1 .. 5], [2 :: Int, 0], [3, 4]] `shouldBe`  [[1, 2, 3, 4, 5], [3 :: Int, 4]]
