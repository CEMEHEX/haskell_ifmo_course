module Tests.Block3Spec where

import           Test.Hspec

import           Block3.Task1
import           Block3.Task2
import           Block3.Task3
import           Block3.Task4
import           Block3.Task5
import           Block3.TreePrinters

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Task1" $ do
    nextDay Sun `shouldBe` Mon
    nextDay Thu `shouldBe` Fri
    afterDays Mon 6 `shouldBe` Sun
    isWeekend Sat `shouldBe` True
    isWeekend Fri `shouldBe` False
    daysToParty Thu `shouldBe` 1

  it "Task2" $ fight arthas cobold `shouldBe` "Winner: {~ Arthas -- health: 1030, attack power: 42, armor: 23 ~}"

  it "Task3" $ do
    len (Vec2D 3 4) `shouldBe` 5.0
    toList (vecSum (Vec3D 1 2 3) (Vec2D 0 1)) `shouldBe` [1, 3, 3]
    scalarMul (Vec3D 1 2 3) (Vec2D 4 2) `shouldBe` 8
    dist (Vec3D 5 5 3) (Vec2D 1 5) `shouldBe` 5.0
    vecMul (Vec2D 1 0) (Vec2D 0 1) `shouldBe` Vec3D 0 0 1

  it "Task4" $ do
    (Z + Z) `shouldBe` Z
    Z + S Z `shouldBe` S Z
    (3 :: Nat) `shouldBe` S (S (S Z))
    (10 :: Nat) - (4 :: Nat) `shouldBe` (6 :: Nat)
    (5 :: Nat) * (3 :: Nat) `shouldBe` (15 :: Nat)
    natGcd 42 84 `shouldBe` 42


  it "Task5" $ do
    insert 5 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf) :: Tree Int) `shouldBe`
      Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 5 Leaf Leaf))
    inTree 123 testTree `shouldBe` True
    inTree 42 testTree `shouldBe` False
