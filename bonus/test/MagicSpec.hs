module MagicSpec where

import           Magic
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "Foldable tests" $
        sum (MagicBox [Just 3, Nothing, Just 1, Just 10]) `shouldBe` 14

    it "Functor tests" $ do
        (2*) <$> MagicBox [Just 3, Nothing, Just 1, Just 10] `shouldBe`
            MagicBox [Just 6, Nothing, Just 2, Just 20]

        (4*) <$> MagicBox (Right [1..5] :: Either String [Int]) `shouldBe`
            MagicBox (Right [4, 8, 12, 16, 20])

    it "Applicative tests" $ do
        (pure 42 :: MagicBox Maybe [] Int) `shouldBe` MagicBox (Just [42])

        MagicBox (Just (Just (+1))) <*> MagicBox (Just (Just 1)) `shouldBe`
            MagicBox (Just (Just 2))

        MagicBox [Just id, Nothing] <*> MagicBox [Just 42, Nothing] `shouldBe`
            MagicBox [Just 42, Nothing, Nothing, Nothing]
    it "Traversable tests" $ do
        sequenceA (MagicBox (Just (Just [1, 2, 3]))) `shouldBe`
            [MagicBox (Just (Just 1)), MagicBox (Just (Just 2)), MagicBox (Just (Just 3))]
