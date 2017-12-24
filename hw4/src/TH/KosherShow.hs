{-# LANGUAGE TemplateHaskell #-}

module TH.KosherShow
    (
      kosherShow
    , deriveKosherShow
    ) where

import           Control.Monad       (mapM, replicateM)
import qualified Data.Text           as T (Text, append, empty, pack)
import           Language.Haskell.TH (Clause, Con (InfixC, NormalC, RecC),
                                      Dec (DataD, FunD, InstanceD), ExpQ,
                                      Info (TyConI), Name, PatQ, Q,
                                      Type (AppT, ConT), clause, conP, nameBase,
                                      newName, normalB, reify, varE, varP)

class KosherShow a where
    kosherShow :: a -> T.Text

data T1

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
    ids <- replicateM n (newName "x")
    return (map varP ids, map varE ids)

genericShowClause :: Name -> [a] -> Q Clause
genericShowClause name fields = do
    let constructorName = nameBase name
    (pats,vars) <- genPE (length fields)

    let f' []       = [| T.pack constructorName |]
        f' (v:vs) = [| $(f' vs) `T.append` T.pack " " `T.append` T.pack (show $v) |]

    let f = f' . reverse

    clause [conP name pats]
           (normalB (f vars)) []

showClause :: Con -> Q Clause
showClause (NormalC name fields) = genericShowClause name fields
showClause (RecC name fields)    = genericShowClause name fields
showClause (InfixC t1 name t2)   = genericShowClause name [t1, t2]
showClause other = error $ "Constructor " ++ show other ++ " is not supported"

deriveKosherShow :: Name -> Q [Dec]
deriveKosherShow t = do
    TyConI (DataD _ _ _ _ constructors _) <- reify t
    showbody <- mapM showClause constructors

    d <- [d| instance KosherShow T1 where
                kosherShow _ = T.empty
           |]

    let    [InstanceD overlap typ (AppT showt (ConT _)) [FunD showf _]] = d
    return [InstanceD overlap typ (AppT showt (ConT t  )) [FunD showf showbody]]
