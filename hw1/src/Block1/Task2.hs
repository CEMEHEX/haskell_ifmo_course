module Block1.Task2 where

highestBit :: (Integral a) => a -> (a, a)
highestBit a | a < 0 = error "Negative argument"
             | otherwise = (2 ^ log2'a, log2'a)
  where
      log2'a = floor $ logBase (2.0 :: Double)  $ fromIntegral a
-- highestBit a = helper 0
--   where
--       helper :: (Integral a) => a -> (a, a)
--       helper cnt | a <= 0 = error "Invalid argument: must be positive"
--                  | a `div` 2 ^ (cnt + 1) == 0 = (2 ^ cnt, cnt)
--                  | otherwise = helper $ cnt + 1
