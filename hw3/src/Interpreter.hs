module Interpreter
    (
      run
    ) where

import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.State.Strict  (evalStateT)

import           Data.Map.Strict             as Map (empty)
import           Data.Text                   as T (Text)

import           Parsing.ConstructionsParser (sourceFileParser)

import           Text.Megaparsec             (runParser)

import           Language.Construction       (runProgram)
import           Language.Utils              (RuntimeError, except, mkExceptIO,
                                              runIOAction, wrapParserOutput)

run :: T.Text -> IO ()
run code = do
    maybeErr <- tryExecute code
    case maybeErr of
        Left e -> print e
        _      -> return ()

tryExecute :: T.Text -> IO (Either RuntimeError ())
tryExecute = runExceptT . interpretFile

interpretFile :: T.Text -> ExceptT RuntimeError IO ()
interpretFile code = do
    program <- mkExceptIO . except . wrapParserOutput $ runParser sourceFileParser "" code
    (evalStateT . runIOAction . runProgram) program empty
