module Block5.Task1 where

import           Data.Either (lefts, rights)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = concat . mconcat

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat xs = (mconcat . lefts $ xs, mconcat . rights $ xs)
