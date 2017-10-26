module Parsec.SExpr where

import           Control.Applicative ((<|>))
import           Parsec.Parser       (Parser, char, ident, oneOrMore, posInt,
                                      skipWS)

type Ident = String

data Atom = N Integer | I Ident
    deriving (Show, Eq)

data SExpr = A Atom
           | Comb [SExpr]
           deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr = skipWS $ parseA <|> parseComb

parseI :: Parser Atom
parseI = I <$> ident

parseN :: Parser Atom
parseN = N <$> posInt

parseAtom :: Parser Atom
parseAtom = parseN <|> parseI

parseA :: Parser SExpr
parseA = A <$> parseAtom

parseComb :: Parser SExpr
parseComb = char '(' *> skipWS (pure Comb) <*> oneOrMore parseSExpr <* skipWS (char ')')
