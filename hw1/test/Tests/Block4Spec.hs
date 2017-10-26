module Tests.Block4Spec where

import           Test.Hspec

import           Block3.Task5  (fromList)
import           Block4.Task1
import           Block4.Task2
import           Data.Foldable (toList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Task1" $ (toList . FTree . fromList $ [3, 1, 5, 4, 2]) `shouldBe` [1 .. 5]

  it "Task2" $ do
    splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
    joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
