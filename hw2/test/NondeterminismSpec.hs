module NondeterminismSpec where

import           Test.Hspec                  (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, choose, forAll)

import           Nondeterminism.BinarySeq    (genBinSeq)
import           Nondeterminism.Combinations (combinations)
import           Nondeterminism.Permutations (permutations)

spec :: Spec
spec = do
    it "Binary sequence simple tests" $ do
        genBinSeq 0 `shouldBe` [[]]
        genBinSeq 1 `shouldBe` [[0], [1]]
        genBinSeq 2 `shouldBe` [[0, 0], [0, 1], [1, 0], [1, 1]]

    describe "BinarySeq" $ do
        prop "binary sequence count" binSeqCntProp
        prop "binary sequence combination" binSeqCombinationProp

    it "Combinations simple tests" $ do
        combinations 1 0 `shouldBe` [[]]
        combinations 0 1 `shouldBe` []
        combinations 3 3 `shouldBe` [[1, 2, 3]]
        combinations 4 2 `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]

    describe "Combinations" $ do
        prop "combinations consistency" combConsistencyProp
        prop "C_n^k formula" combFormulaProp
        prop "sum C_n^k = 2 ^ n" combSumProp
        prop "C_n^0 = C_n^n" combSimple1Prop
        prop "C_(n + 1)^k = C_n^(k - 1) + C_n^k" combSimple2Prop

    it "Permutations simple tests" $ do
        permutations ([] :: [()]) `shouldBe` [[]]
        permutations [1,2,3] `shouldBe` [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
        permutations [[1,2,3], [4,5,6]] `shouldBe` [[[1,2,3], [4,5,6]],[[4,5,6],[1,2,3]]]

    describe "Permutations" $ do
        prop "permutations consistency" permConsistencyProp
        prop "permutations count" permCntProp

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

binSeqCntProp :: Property
binSeqCntProp =
    forAll (choose (0 :: Int, 10)) $ \n ->
    length (genBinSeq n) == 2 ^ n

binSeqCombinationProp :: Property
binSeqCombinationProp =
    forAll (choose (0 :: Int, 9)) $ \n ->
    forAll (choose (0 :: Int, 9)) $ \m ->
    (genBinSeq n >>= \x -> genBinSeq m >>= \y -> return $ x ++ y) == genBinSeq (n + m)

combConsistencyProp :: Property
combConsistencyProp =
    forAll (choose (0 :: Int, 10)) $ \n ->
    forAll (choose (0 :: Int, 10)) $ \k ->
    all (`elem` [1..n]) $ concat $ combinations n k

combFormulaProp :: Property
combFormulaProp =
    forAll (choose (0 :: Int, 10)) $ \n ->
    forAll (choose (0 :: Int, 10)) $ \k ->
    n < k || length (combinations n k) == (fac n `div` (fac k * fac (n - k)))

combSumProp :: Property
combSumProp =
    forAll (choose (0 :: Int, 10)) $ \n ->
    sum (length . combinations n <$> [0..n]) == 2 ^ n

combSimple2Prop :: Property
combSimple2Prop =
    forAll (choose (0 :: Int, 10)) $ \n ->
    forAll (choose (0 :: Int, 10)) $ \k ->
    length (combinations (n + 1) k) ==
        length (combinations n (k - 1)) + length (combinations n k)

combSimple1Prop :: Property
combSimple1Prop =
    forAll (choose (0 :: Int, 10)) $ \n ->
    length (combinations n 0) == length (combinations n n)

permCntProp :: Property
permCntProp =
    forAll (choose (0 :: Int, 8)) $ \n ->
    length (permutations [1..n]) == fac n

permConsistencyProp :: Property
permConsistencyProp =
    forAll (choose (0 :: Int, 8)) $ \n ->
    all (all $ flip elem [1..n]) $ permutations [1..n]
