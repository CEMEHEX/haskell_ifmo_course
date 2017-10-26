module Block1.Task4 where

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains = filter . elem
