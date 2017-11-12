{-# LANGUAGE OverloadedStrings #-}

module Main
    (
      main
    ) where

import qualified Data.Text          as T (Text)
import qualified Data.Text.IO       as TIO (putStrLn, readFile)

import           System.Environment (getArgs)

import           Interpreter        (run, startInteractive)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> startInteractive
        1 -> TIO.readFile (head args) >>= run
        _ -> TIO.putStrLn usage

usage :: T.Text
usage = "USAGE: marik [path to program]"
