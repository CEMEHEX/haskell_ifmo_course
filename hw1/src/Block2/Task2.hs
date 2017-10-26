module Block2.Task2 where

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery k list = helper 0 list [] []
  where
      helper :: Int -> [a] -> [a] -> [a] -> ([a], [a])
      helper _ [] collected rest = (reverse rest, reverse collected)
      helper i (x:xs) collected rest = helper (mod (i + 1) k) xs newCollected newRest
        where
            (newCollected, newRest) = if i + 1 == k then (x:collected, rest) else (collected, x:rest)
