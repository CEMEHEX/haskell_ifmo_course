module OptionalMonadsSpec where

import           Test.Hspec            (Spec, hspec, it, shouldBe)

import           OptionalMonads.Arithm (ArithmeticError (..), Expr (..), eval)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "ArithmSuccess" $ do
      eval (Const 42) `shouldBe` Right 42
      eval (Addition (Const 24) (Const 6)) `shouldBe` Right 30
      eval (Multipliation (Const 1005) (Const 100)) `shouldBe` Right 100500
      eval (Subtraction (Const 120) (Const 9)) `shouldBe` Right 111
      eval (Division (Const 126) (Const 3)) `shouldBe` Right 42
      eval (Power (Const 4) (Const 3)) `shouldBe` Right 64
      eval (Addition (Const 6) (Power (Const 6) (Division (Addition (Const 13) (Const 17))
           (Subtraction (Const 20) (Const 5))))) `shouldBe` Right 42
  it "ArithmMustFail" $ do
      eval (Division (Const 1) (Const 0)) `shouldBe` Left DivByZero
      eval (Power (Const 42) (Const (-42))) `shouldBe` Left NegativeExp
      eval (Addition (Const 6) (Power (Const 6) (Division (Addition (Const 13) (Const 17))
           (Subtraction (Const 5) (Const 20))))) `shouldBe` Left NegativeExp
      eval (Addition (Const 6) (Power (Const 6) (Division (Addition (Const 13) (Const 17))
           (Subtraction (Const 20) (Const 20))))) `shouldBe` Left DivByZero
