{-# LANGUAGE FlexibleInstances #-}

module Language.Construction
    (
      Statement (..)
    , Program
    , runProgram
    ) where

import           Control.Monad.State.Strict (get, lift)

import           Language.Expression        (Expr, getResult)
import           Language.MutableVar        (Command (..), create, delete,
                                             update)
import           Language.Utils             (VarName)

type Program a = [Statement a]

data Statement a
    = New VarName (Expr a)
    | Upd VarName (Expr a)
    | Out (Expr a)
    | In VarName
    | For VarName a a (Program a)
    deriving (Show, Eq)

runProgram :: Program Integer -> Command Integer
runProgram program = Command $ mapM_ (runCmd . runStatement) program

runStatement :: Statement Integer -> Command Integer
runStatement (New name expr) = Command $ do
    m <- get
    result <- lift $ getResult expr m
    runCmd $ create name result
runStatement (Upd name expr) = Command $ do
    m <- get
    result <- lift $ getResult expr m
    runCmd $ update name result
runStatement (Out _) = undefined -- TODO
runStatement (In _)  = undefined -- TODO
runStatement (For name begin end program) = Command $ do
    let mkIteration i = Command $ do {
        runCmd $ create name i;
        runCmd $ runProgram program;
    }
    mapM_ (runCmd . mkIteration) [begin..end]
    runCmd $ delete name
