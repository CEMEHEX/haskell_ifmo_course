module Block1.Task3 where

import           Data.List (genericReplicate)

smartReplicate :: (Integral a) => [a] -> [a]
smartReplicate = concatMap selfReplicate
  where
      selfReplicate :: (Integral a) => a -> [a]
      selfReplicate n = genericReplicate n n
