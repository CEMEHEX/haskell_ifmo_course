module Interpreter
    (
      tryExecute
    , getTree
    ) where

import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.State.Strict  (evalStateT)

import           Data.Map.Strict             as Map (empty)
import           Data.Text                   as T (Text)

import           Parsing.ConstructionsParser (sourceFileParser)

import           Text.Megaparsec             (parseTest, runParser)

import           Language.Construction       (runProgram)
import           Language.Utils              (RuntimeError, except, mkExceptIO,
                                              runIOAction, wrapParserOutput)

getTree :: T.Text -> IO ()
getTree = parseTest sourceFileParser

tryExecute :: T.Text -> IO (Either RuntimeError ())
tryExecute = runExceptT . interpretFile

interpretFile :: T.Text -> ExceptT RuntimeError IO ()
interpretFile code = do
    program <- mkExceptIO . except . wrapParserOutput $ runParser sourceFileParser "" code
    (evalStateT . runIOAction . runProgram) program empty
