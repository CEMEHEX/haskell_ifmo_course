{-# LANGUAGE OverloadedStrings #-}

module Parsing.VarDeclParser
    (
      varDeclParser
    ) where

import           Control.Applicative   ((<|>))

import           Language.Construction (VarDecl (..))
import           Language.Expression   (Expr)

import           Parsing.ExprParser    (exprParser)
import           Parsing.Utils         (Parser, identifier, rword, symbol)

varDeclParser :: Parser (VarDecl (Expr Integer))
varDeclParser =
    New <$ rword "mut" <*> identifier <* symbol "=" <*> exprParser <|>
    Upd <$> identifier <* symbol "=" <*> exprParser
