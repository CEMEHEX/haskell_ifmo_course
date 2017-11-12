{-# LANGUAGE OverloadedStrings #-}

module Parsing.ExprParser
    (
      exprParser
    ) where

import           Control.Applicative  ((<|>))
import           Text.Megaparsec.Expr (Operator (..), makeExprParser)

import           Language.Expression  (Expr (..))
import           Parsing.Utils        (Parser, identifier, integer, parens,
                                       rword, symbol)


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
