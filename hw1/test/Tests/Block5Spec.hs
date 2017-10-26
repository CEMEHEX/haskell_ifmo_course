module Tests.Block5Spec where

import           Test.Hspec

import           Block5.Task1
import           Block5.Task2
import           Block5.Task3
import           Data.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Task1" $ do
    maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5]
    eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] `shouldBe` (Sum {getSum = 8}, [1,2,3,4,5])

  it "Task2" $ do
    1 `shouldBe` 1

  it "Task3" $ do
    1 `shouldBe` 1
