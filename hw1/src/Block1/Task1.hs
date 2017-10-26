module Block1.Task1 where

import           Data.List (sort)

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (\ [a', b', c'] -> (a', b', c')) $ sort [a, b, c]

-- order3 (a, b, c) = (min a $ min b c,
--                     max (min a b) (min a c),
--                     max a $ max b c)
