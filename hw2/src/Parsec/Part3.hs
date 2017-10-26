module Parsec.Part3 where

import           Control.Applicative ((<|>))
import           Parsec.Parser       (Parser, char, ident, oneOrMore, posInt,
                                      sepBy, spaces, string)

type VarName = String

data Var = Var VarName | Const Integer

data Let = Let { var  :: VarName
               , vars :: [Var]
               }

newtype LetExpr = LetExpr [Let]

parseVar :: Parser Var
parseVar = Var <$> ident <|> Const <$> posInt

parseLet :: Parser Let
parseLet =
    spaces >>
    string "let" >>
    spaces >>
    ident >>= \x ->
    spaces >>
    char '=' >>
    (parseVar `sepBy` "+") >>= \xs ->
    return $ Let x xs

parseLetExpr :: Parser LetExpr
parseLetExpr = LetExpr <$> oneOrMore parseLet

instance Show LetExpr where
    show (LetExpr lets) = foldr1 (\a b -> a ++ '\n' : b) $ show <$> lets

instance Show Let where
    show expr = "let " ++ var expr ++ " = " ++
        foldr1 (\a b -> a ++ " + " ++ b) (show <$> vars expr)

instance Show Var where
    show (Var name) = name
    show (Const n)  = show n
