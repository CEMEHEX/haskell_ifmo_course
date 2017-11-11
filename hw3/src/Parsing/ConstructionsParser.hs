{-# LANGUAGE OverloadedStrings #-}

module Parsing.ConstructionsParser
    (
      sourceFileParser
    ) where

import           Control.Applicative ((<|>))

import           Language.Utils      (Program, Statement (..))

import           Parsing.ExprParser  (exprParser)
import           Parsing.Utils       (Parser, codeBlock, identifier, integer,
                                      rword, spaceConsumer, symbol)

import           Text.Megaparsec     (between, eof, many)


varDeclParser :: Parser (Statement Integer)
varDeclParser =
    New <$ rword "mut" <*> identifier <* symbol "=" <*> exprParser <|>
    Upd <$> identifier <* symbol "=" <*> exprParser

inParser :: Parser (Statement Integer)
inParser = In <$ symbol ">" <*> identifier

outParser :: Parser (Statement Integer)
outParser = Out <$ symbol "<" <*> exprParser

forParser :: Parser (Statement Integer)
forParser =
    For <$ rword "for" <*> identifier <* rword "in" <*>
    integer <* symbol ".." <*>
    integer <*> codeBlock programParser

statementParser :: Parser (Statement Integer)
statementParser =  varDeclParser <|> inParser <|> outParser <|> forParser

programParser :: Parser (Program Integer)
programParser = many statementParser

sourceFileParser :: Parser (Program Integer)
sourceFileParser = between spaceConsumer eof programParser
