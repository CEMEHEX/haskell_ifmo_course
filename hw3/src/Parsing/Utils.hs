{-# LANGUAGE OverloadedStrings #-}

module Parsing.Utils
    (
      Parser
      , identifier
      , integer
      , parens
      , spaceConsumer
      , symbol
      , rword
    ) where

import           Data.Text                  (Text, cons, pack)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, many,
                                             notFollowedBy, try)
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space1,
                                             string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme,
                                                  skipBlockComment,
                                                  skipLineComment, space,
                                                  symbol)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

rword :: Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [Text] -- list of reserved words
rws = ["let", "in", "mut"]

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p       = cons <$> letterChar <*> (pack <$> many alphaNumChar)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
