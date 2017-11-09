{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

module Language.Construction
    (
      VarDecl (..)
    , Executable
    , runExecutable
    , runProgram
    ) where

import           Control.Monad.State.Strict
import           Language.Expression        (Expr, getResult)
import           Language.MutableVar        (Command (..), create, update)
import           Language.Utils

-- TODO rename all this ugly shit

data Program = forall e . (Executable e) => Program [e]

data VarDecl a = New VarName a | Upd VarName a
    deriving (Show, Eq)

runProgram :: Program -> Command Integer
runProgram (Program execs) = Command $ mapM_ (runCmd . runExecutable) execs

class Executable e where
    runExecutable :: e -> Command Integer

instance Executable (VarDecl (Expr Integer)) where
    runExecutable (New name expr) = Command $ do
        m <- get
        result <- lift $ getResult expr m
        runCmd $ create name result
    runExecutable (Upd name expr) = Command $ do
        m <- get
        result <- lift $ getResult expr m
        runCmd $ update name result
