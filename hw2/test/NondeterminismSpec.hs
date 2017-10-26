module NondeterminismSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Nondeterminism.BinarySeq
import           Nondeterminism.Combinations
import           Nondeterminism.Permutations

spec :: Spec
spec = do
    describe "BinarySeq" $ do
        prop "binary sequence count" binSeqCntProp
        prop "binary sequence combination" binSeqCombinationProp

    describe "Combinations" $ do
        prop "combinations consistency" combConsistencyProp
        prop "C_n^k formula" combFormulaProp
        prop "sum C_n^k = 2 ^ n" combSumProp
        prop "C_n^0 = C_n^n" combSimple1Prop
        prop "C_(n + 1)^k = C_n^(k - 1) + C_n^k" combSimple2Prop

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
