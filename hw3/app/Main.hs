module Main where

import           Data.Text.IO                as T (readFile)
import           Parsing.ConstructionsParser (programParser)
import           Text.Megaparsec             (runParser)

main :: IO ()
main = do
    -- filePath <- getLine
    let filePath = "sample/test.sy"
    code <- T.readFile filePath
    print $ runParser programParser "" code
