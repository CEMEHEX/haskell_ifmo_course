module Nondeterminism.Permutations where

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations list =
    partitions list >>= \ (x, xs) ->
    permutations xs >>= \ ys ->
    return $ x : ys
    where
        partitions :: [a] -> [(a, [a])]
        partitions []     = []
        partitions (x:xs) =
          (x, xs) : (partitions xs >>= \ (y, ys) -> return (y, x:ys))

-- partitions :: [a] -> [a] -> [(a, [a])]
-- partitions [] _        = []
-- partitions (x:xs) rest = (x, rest ++ xs) : partitions xs (rest ++ [x])
