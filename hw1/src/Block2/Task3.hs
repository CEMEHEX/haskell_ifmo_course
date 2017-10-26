module Block2.Task3 where

import           Control.Exception (SomeException, catch)
import           Data.Char         (isDigit)

stringSum :: (Num a, Read a) => String -> a
stringSum = sum . map (read . removeUnaryPlus) . words
  where
      removeUnaryPlus :: String -> String
      removeUnaryPlus "+" = error "+ is not a number"
      removeUnaryPlus ('+':x:xs) = if isDigit x then x:xs else error "Invalid character after '+' sign"
      removeUnaryPlus s = s

runTests :: [a] -> (a -> IO ()) -> IO ()
runTests tests action = mapM_ (\x -> catch (action x) handler) tests
  where
      handler :: SomeException -> IO ()
      handler _ = print "failed"

passTests :: [String]
passTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
            , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
            , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
            ]

mustFail :: [String]
mustFail  = ["asd", "1-1", "1.2", "--2", "+1", "1+"]

advancedTests :: [String]
advancedTests    = [ "+1", "1 +1", "-1 +1", "+1 -1"]

advancedMustFail :: [String]
advancedMustFail = ["1+1", "++1", "-+1", "+-1", "1 + 1"]
