module Block4.Task2 where

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delimiter = foldr addNext [[]]
  where
    addNext _ [] = error "something strange happened"
    addNext element res@(x:xs) | element == delimiter = [] : res
                               | otherwise = (element:x):xs

joinWith :: a -> [[a]] -> [a]
joinWith delimiter = foldr1 (\a b -> a ++ delimiter:b)
