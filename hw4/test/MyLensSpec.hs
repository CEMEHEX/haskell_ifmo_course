module MyLensSpec
    (
      spec
    ) where

import           Test.Hspec

import           MyLenses.BaseLens

spec :: Spec
spec = do
    it "view" $ do
        (239 :: Int, 42 :: Int)^._1 `shouldBe` 239
        (239 :: Int, 42 :: Int)^._2 `shouldBe` 42

    it "set" $ do
        (_1 .~ 239) (0 :: Int, 42 :: Int) `shouldBe` (239, 42)
        (_2 .~ 42) (239 :: Int, 0 :: Int) `shouldBe` (239, 42)

    it "over" $ do
        (_1 %~  (+100)) (139 :: Int, 42 :: Int) `shouldBe` (239, 42)
        (_2 %~ (*2)) (239 :: Int, 21 :: Int) `shouldBe` (239, 42)
