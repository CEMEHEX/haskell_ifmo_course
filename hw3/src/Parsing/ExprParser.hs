{-# LANGUAGE OverloadedStrings #-}

module Parsing.ExprParser
    (
      exprParser
    ) where

import           Construction.Expression (Expr (..))
import           Control.Applicative     ((<|>))
import           Parsing.Utils           (Parser, identifier, integer, parens,
                                          rword, symbol)
import           Text.Megaparsec.Expr    (Operator (..), makeExprParser)


-- whileParser :: Parser Expr
-- whileParser = between spaceConsumer eof exprParser

exprParser :: Parser (Expr Integer)
exprParser = makeExprParser arithmTerm arithmOperators

arithmOperators :: [[Operator Parser (Expr Integer)]]
arithmOperators =
    [ [InfixL (Mul <$ symbol "*"), InfixL (Div <$ symbol "/")]
    , [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]
    ]

arithmTerm :: Parser (Expr Integer)
arithmTerm =
    Lit <$> integer <|>
    Var <$> identifier <|>
    parens (exprParser <|>
    Let <$ rword "let" <*> identifier <* symbol "="
        <*> exprParser <* rword "in"
        <*> exprParser)
