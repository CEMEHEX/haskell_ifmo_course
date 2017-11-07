{-# LANGUAGE OverloadedStrings #-}

module Parsing.ExprParser
    (
      exprParser
    ) where

import           Construction.Expression (Expr (..))
import           Control.Applicative     ((<|>))
import           Parsing.Utils           (Parser, identifier, integer, parens,
                                          rword, spaceConsumer, symbol)
import           Text.Megaparsec         (between, eof)
import           Text.Megaparsec.Expr    (Operator (..), makeExprParser)


whileParser :: Parser Expr
whileParser = between spaceConsumer eof exprParser

exprParser :: Parser Expr
exprParser = makeExprParser arithmTerm arithmOperators

arithmOperators :: [[Operator Parser Expr]]
arithmOperators =
    [ [InfixL (Mul <$ symbol "*"), InfixL (Div <$ symbol "/")]
    , [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]
    ]

arithmTerm :: Parser Expr
arithmTerm =
    Lit <$> integer <|>
    Var <$> identifier <|>
    parens (exprParser <|>
    Let <$ rword "let" <*> identifier <* symbol "="
        <*> exprParser <* rword "in"
        <*> exprParser)
