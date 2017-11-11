{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text          as T (Text)
import qualified Data.Text.IO       as TIO (putStrLn, readFile)
import           Interpreter        (run)
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then TIO.putStrLn usage
        else TIO.readFile (head args) >>= run

usage :: T.Text
usage = "USAGE: marik <path to program>"
