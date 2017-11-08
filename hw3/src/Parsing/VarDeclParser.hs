{-# LANGUAGE OverloadedStrings #-}

module Parsing.VarDeclParser
    (
      varDeclParser
    ) where

import           Construction.Variable (VarDecl (..))
import           Control.Applicative   ((<|>))
import           Parsing.Utils

varDeclParser :: Parser (VarDecl Integer)
varDeclParser =
    New <$ rword "mut" <*> identifier <* symbol "=" <*> integer <|>
    Upd <$> identifier <* symbol "=" <*> integer
