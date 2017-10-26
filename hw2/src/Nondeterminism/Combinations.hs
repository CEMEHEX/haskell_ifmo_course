module Nondeterminism.Combinations where

combinations :: (Integral a) => a -> a -> [[a]]
combinations n = helper [1..n]
  where
      helper _ 0 = [[]]
      helper [] _ = []
      helper rest k =
          rest >>= \x ->
          helper (dropWhile (<= x) rest) (k - 1) >>= \xs ->
          return $ x:xs
