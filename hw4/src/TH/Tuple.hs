module TH.Tuple
    (
      choseByIndices
    ) where

import           Language.Haskell.TH (Exp, Q, appE, lamE, newName, tupE, tupP,
                                      varE, varP, wildP)

projection :: Int
           -> Q Exp -- tuple
           -> Int
           -> Q Exp
projection n tuple i = do
    x <- newName "x"
    appE (lamE [tupP $ replicate (i - 1) wildP ++ [varP x] ++ replicate (n - i) wildP] (varE x)) tuple

choseByIndices :: Int
               -> [Int]
               -> Q Exp
choseByIndices n indices = do
    t <- newName "t"
    lamE [varP t] (tupE $ map (projection n (varE t)) indices)
