module Block2.Task4 where

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = mergeSort left `merge` mergeSort right
  where
      left = take lenDiv2 list
      right = drop lenDiv2 list
      lenDiv2 = div (length list) 2

      merge :: (Ord a) => [a] -> [a] -> [a]
      merge xs [] = xs
      merge [] ys = ys
      merge (x:xs) (y:ys) | x < y = x:merge xs (y:ys)
                          | otherwise = y:merge (x:xs) ys
