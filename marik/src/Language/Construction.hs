{-# LANGUAGE FlexibleInstances #-}

module Language.Construction
    (
      runProgram
    ) where

import           Control.Monad.Except       (Except, ExceptT)
import           Control.Monad.State.Strict (StateT, get, lift)
import           Data.Text.IO               as T (getLine)

import           Language.Expression        (getResult)
import           Language.MutableVar        (create, delete, update)
import           Language.Utils             (Command (..), Expr, IOAction (..),
                                             NameToVal, Program, RuntimeError,
                                             Statement (..), VarName, except,
                                             mkIOAction, wrapParserOutput)

import           Parsing.ExprParser         (exprParser)
import           Text.Megaparsec            (runParser)
-- TODO improve error messages
runProgram :: Program Integer -> IOAction Integer ()
runProgram program = IOAction $ mapM_ (runIOAction . runStatement) program

runStatement :: Statement Integer -> IOAction Integer ()
runStatement (New name expr) = mkIOAction . Command $ do
    m <- get
    result <- lift . except $ getResult expr m
    runCmd $ create name result
runStatement (Upd name expr) = mkIOAction . Command $ do
    m <- get
    result <- lift . except $ getResult expr m
    runCmd $ update name result
runStatement (Out expr) = IOAction $ do
    result <- runIOAction . calculate $ expr
    lift . lift $ print result
runStatement (In name)  = IOAction $ do
    value <- lift . lift $ T.getLine
    expr <- runIOAction . parseInput $ value
    result <- runIOAction . calculate $ expr
    addIO . runCmd $ update name result
runStatement (For name beginExpr endExpr program) = IOAction $ do
    begin <- runIOAction . calculate $ beginExpr
    end <- runIOAction . calculate $ endExpr

    runIOAction . mkIOAction $ create name begin

    let mkIteration i = IOAction $ do {
        runIOAction . mkIOAction $ update name i;
        runIOAction $ runProgram program;
    }
    mapM_ (runIOAction . mkIteration) $ range begin end

    runIOAction . mkIOAction $ delete name

calculate :: Expr Integer -> IOAction Integer Integer
calculate expr = IOAction $ do
    m <- get
    addIO . lift . except $ getResult expr m

parseInput :: VarName -> IOAction Integer (Expr Integer)
parseInput input = IOAction $
    addIO . lift . except . wrapParserOutput $ runParser exprParser "" input

-- like mkIOAction, but for internal part of Command
addIO :: StateT (NameToVal a) (Except RuntimeError) b
      -> StateT (NameToVal a) (ExceptT RuntimeError IO) b
addIO = runIOAction . mkIOAction . Command

range :: (Ord a, Enum a) => a -> a -> [a]
range begin end = if begin <= end
    then [begin..end]
    else [begin, pred begin..end]
