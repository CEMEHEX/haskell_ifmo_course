module BooleanTableSpec where

import           BooleanTable
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "One argument" $
        getRows (booleanTable not) `shouldBe` [([False],True),([True],False)]
    it "Two arguments" $
        getRows (booleanTable (||)) `shouldBe`
            [([False,False],False),([False,True],True),([True,False],True),([True,True],True)]
    it "Three arguments" $
        getRows (booleanTable (\a b c -> a && b || c)) `shouldBe`
          [([False,False,False],False),([False,False,True],True),
          ([False,True,False],False),([False,True,True],True),
          ([True,False,False],False),([True,False,True],True),
          ([True,True,False],True),([True,True,True],True)]
