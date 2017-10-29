module MonstuparSpec where

import           Data.Char         (isUpper)
import           Monstupar.LetExpr (Let (..), LetExpr (..), OptimizedLet (..),
                                    OptimizedLetExpr (..), Var (..),
                                    parseAndOptLetExpr, parseLetExpr)
import           Monstupar.Parser  (ParseError (..), ident, oneOrMore,
                                    runParser, satisfy, zeroOrMore)
import           Monstupar.SExpr   (Atom (..), SExpr (..), parseSExpr)
import           Monstupar.Simple  (abParser, abParser_, intOrUpperCase,
                                    intPair)
import           Test.Hspec        (Spec, it, shouldBe)

spec :: Spec
spec = do
    it "simple parsers tests" $ do
        runParser abParser "" `shouldBe` Left ParseError
        runParser abParser "abcdef" `shouldBe` Right (('a','b'),"cdef")
        runParser abParser "aebcdf" `shouldBe` Left ParseError

        runParser abParser_ "" `shouldBe` Left ParseError
        runParser abParser_ "abcdef" `shouldBe` Right ((),"cdef")
        runParser abParser_ "aebcdf" `shouldBe` Left ParseError

        runParser intPair "" `shouldBe` Left ParseError
        runParser intPair "12 34" `shouldBe` Right ([12,34],"")

        runParser intOrUpperCase "" `shouldBe` Left ParseError
        runParser intOrUpperCase "342abcd" `shouldBe` Right ((), "abcd")
        runParser intOrUpperCase "XYZ" `shouldBe` Right ((), "YZ")
        runParser intOrUpperCase "foo" `shouldBe` Left ParseError

    it "SExpr parser" $ do
        runParser parseSExpr "" `shouldBe` Left ParseError
        runParser parseSExpr "5" `shouldBe` Right (A (N 5), "")
        runParser parseSExpr " \t  5" `shouldBe` Right (A (N 5), "")
        runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe`
            Right (Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)], "")
        runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" `shouldBe`
            Right (Comb [A (I "lots"), A (I "of"), Comb [A (I "spaces"), A (I "in")],
             A (I "this"), Comb [A (I "one")]], "")
        runParser parseSExpr "( lots of ( spaces in ) this ( one ) " `shouldBe` Left ParseError

    it "others from task2" $ do
        runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Right ("ABC","dEfgH")
        runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Right ("ABC","dEfgH")

        runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe`
            Right ("","abcdeFGh")
        runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe`
            Left ParseError

        runParser ident "" `shouldBe` Left ParseError
        runParser ident "foobar baz" `shouldBe` Right ("foobar"," baz")
        runParser ident "foo33fA" `shouldBe` Right ("foo33fA","")
        runParser ident "2bad" `shouldBe` Left ParseError

    it "let expressions parser" $ do
        runParser parseLetExpr "" `shouldBe` Left ParseError
        runParser parseLetExpr "let x = 1" `shouldBe` Right (LetExpr
            [Let "x" [Const 1]], "")
        runParser parseLetExpr
            "  let   x  = 1  \n\t let    y  = x + x  \n \t \t\r let z=y+1" `shouldBe`
            Right (LetExpr [Let "x" [Const 1], Let "y" [Var "x", Var "x"],
            Let "z" [Var "y", Const 1]], "")

    it "let expressions optimizator" $ do
        runParser parseAndOptLetExpr "" `shouldBe` Left ParseError
        runParser parseAndOptLetExpr "let x=1" `shouldBe`
            Right (OptLetExpr [OptLet "x" 1], "")
        runParser parseAndOptLetExpr
            "let \tx = 1 + 2 + 5\n   let   y = x+x  \n\r\tlet z=0+    x   + y + 8"
            `shouldBe`
            Right (OptLetExpr [OptLet "x" 8, OptLet "y" 16, OptLet "z" 32], "")
