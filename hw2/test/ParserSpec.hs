module ParserSpec where

import           Data.Char     (isUpper)
import           Parsec.Parser (ident, oneOrMore, runParser, satisfy,
                                zeroOrMore)
import           Parsec.SExpr  (Atom (..), SExpr (..), parseSExpr)
import           Parsec.Simple (abParser, abParser_, intOrUpperCase, intPair)
import           Test.Hspec    (Spec, it, shouldBe)

spec :: Spec
spec = do
    it "simple parsers tests" $ do
        runParser abParser "abcdef" `shouldBe` Just (('a','b'),"cdef")
        runParser abParser "aebcdf" `shouldBe` Nothing
        runParser abParser_ "abcdef" `shouldBe` Just ((),"cdef")
        runParser abParser_ "aebcdf" `shouldBe` Nothing
        runParser intPair "12 34" `shouldBe` Just ([12,34],"")
        runParser intOrUpperCase "342abcd" `shouldBe` Just ((), "abcd")
        runParser intOrUpperCase "XYZ" `shouldBe` Just ((), "YZ")
        runParser intOrUpperCase "foo" `shouldBe` Nothing

    it "SExpr parser" $ do
        runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")
        runParser parseSExpr " \n\t  5" `shouldBe` Just (A (N 5), "")
        runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe`
            Just (Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)], "")
        runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" `shouldBe`
            Just (Comb [A (I "lots"), A (I "of"), Comb [A (I "spaces"), A (I "in")],
             A (I "this"), Comb [A (I "one")]], "")

    it "others from task2" $ do
        runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Just ("ABC","dEfgH")
        runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Just ("ABC","dEfgH")
        runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe`
            Just ("","abcdeFGh")
        runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe`
            Nothing
        runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
        runParser ident "foo33fA" `shouldBe` Just ("foo33fA","")
        runParser ident "2bad" `shouldBe` Nothing
        runParser ident "" `shouldBe` Nothing
