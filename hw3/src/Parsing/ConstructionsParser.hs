{-# LANGUAGE OverloadedStrings #-}

module Parsing.ConstructionsParser
    (
      programParser
    ) where

import           Control.Applicative   ((<|>))

import           Language.Construction (Program, Statement (..))

import           Parsing.ExprParser    (exprParser)
import           Parsing.Utils         (Parser, identifier, rword, symbol)

import           Text.Megaparsec       (many)


varDeclParser :: Parser (Statement Integer)
varDeclParser =
    New <$ rword "mut" <*> identifier <* symbol "=" <*> exprParser <|>
    Upd <$> identifier <* symbol "=" <*> exprParser

inParser :: Parser (Statement Integer)
inParser = In <$ symbol ">" <*> identifier

outParser :: Parser (Statement Integer)
outParser = Out <$ symbol "<" <*> exprParser

statementParser :: Parser (Statement Integer)
statementParser =  varDeclParser <|> inParser <|> outParser

programParser :: Parser (Program Integer)
programParser = many statementParser
