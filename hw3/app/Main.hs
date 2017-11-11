module Main where

import qualified Data.Text.IO as T (readFile)
import           Interpreter  (tryExecute)

main :: IO ()
main = do
    filePath <- getLine
    code <- T.readFile filePath
    maybeErr <- tryExecute code
    case maybeErr of
        Left e -> print e
        _      -> return ()
