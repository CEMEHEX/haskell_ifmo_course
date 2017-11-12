{-# LANGUAGE OverloadedStrings #-}

module Interpreter
    (
      run
    , startInteractive
    ) where

import           Control.Monad               (void)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.State.Strict  (execStateT)

import qualified Data.Map.Strict             as Map (empty)
import qualified Data.Text                   as T (Text)
import qualified Data.Text.IO                as TIO (getLine, putStr, putStrLn)

import           System.IO                   (hFlush, stdout)

import           Parsing.ConstructionsParser (sourceFileParser)

import           Text.Megaparsec             (runParser)

import           Language.Core               (runProgram)
import           Language.Utils              (Code, NameToVal, RuntimeError,
                                              except, mkExceptIO, runIOAction,
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
    case resOrErr of
        Left e   -> print e >> return m
        Right m' -> return m'


tryInterpret :: NameToVal Integer
             -> Code
             -> IO (Either RuntimeError (NameToVal Integer))
tryInterpret m code = runExceptT $ do
    program <- mkExceptIO . except . wrapParserOutput $ runParser sourceFileParser "" code
    (execStateT . runIOAction . runProgram) program m
