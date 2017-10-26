module Nondeterminism.BinarySeq where

genBinSeq :: (Integral a) => a -> [[Int]]
genBinSeq 0 = [[]] -- (1): definition for 0
genBinSeq n =      -- (2): definition for n
    [0, 1] >>= \x ->
    genBinSeq (n - 1) >>= \xs ->
    return $ x:xs

-- genBinSeq n >>= \x -> genBinSeq m >>= \y -> return $ x ++ y === genBinSeq (n + m) (Th)

-- genBinSeq 0 >>= \x -> genBinSeq n >>= \y -> return $ x ++ y === genBinSeq n (zeroConcat)

-- genBinSeq n === genBinSeq 1 >>= \x -> genBinSeq (n - 1) >>= \y -> return $ x ++ y (composition\decomposition)


{- Proof of zeroConcat:
    genBinSeq 0 >>= \x -> genBinSeq n >>= \y -> return $ x ++ y =>
    [[]] >>= \x -> genBinSeq n >>= \y -> return $ x ++ y => (1): definition for 0
    genBinSeq n >>= \y -> return $ [] ++ y => (definition of bind)
    genBinSeq n >>= \y -> return y => (definition of ++)
    genBinSeq n >>= return => η - reduction
    genBinSeq n
-}

{- Proof of composition\decomposition
    genBinSeq n
    => (2): definition for n
    [0, 1] >>= \x ->
    genBinSeq (n - 1) >>= \xs ->
    return $ x:xs
    => (obvious law for lists: x:xs === [x] ++ [xs])
    [[0], [1]] >>= \x ->
    genBinSeq (n - 1) >>= \y ->
    return $ x ++ y
    => (reverse beta reduction of genBinSeq 1)
    genBinSeq 1 >>= \x -> genBinSeq (n - 1) >>= \y -> return $ x ++ y
-}

-- Proof of Th: composition\decomposition => Th
-- (Sequential application of decomposition to second argument and composition to first argument)
