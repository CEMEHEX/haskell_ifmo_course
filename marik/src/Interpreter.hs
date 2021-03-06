{-# LANGUAGE OverloadedStrings #-}

module Interpreter
    (
      run
    , startInteractive
    ) where

import           Control.Monad               (void)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.State.Strict  (execStateT)

import           Data.Either                 (either)
import qualified Data.Map.Strict             as Map (empty)
import qualified Data.Text                   as T (Text)
import qualified Data.Text.IO                as TIO (getLine, putStr, putStrLn)

import           System.IO                   (hFlush, stdout)

import           Text.Megaparsec             (runParser)

import           Parsing.ConstructionsParser (sourceFileParser)

import           Language.Core               (runProgram)
import           Language.Utils              (Code, NameToVal, RuntimeError,
                                              mkExceptIO, runIOAction,
                                              wrapParserOutput)

startInteractive :: IO ()
startInteractive = interactiveStep Map.empty

interactiveStep :: NameToVal Integer -> IO ()
interactiveStep m = do
    cmd <- prompt "meow:3 "
    if cmd == ":q"
        then TIO.putStrLn "purr... purr..."
        else runWithState m cmd >>= interactiveStep

prompt :: T.Text -> IO Code
prompt text = do
    TIO.putStr text
    hFlush stdout
    TIO.getLine

run :: Code -> IO ()
run = void . runWithState Map.empty

runWithState :: NameToVal Integer -> Code -> IO (NameToVal Integer)
runWithState m code = do
    resOrErr <- tryInterpret m code
    either (\e -> print e >> return m) return resOrErr

tryInterpret :: NameToVal Integer
             -> Code
             -> IO (Either RuntimeError (NameToVal Integer))
tryInterpret m code = runExceptT $ do
    program <- mkExceptIO . wrapParserOutput $ runParser sourceFileParser "" code
    (execStateT . runIOAction . runProgram) program m
