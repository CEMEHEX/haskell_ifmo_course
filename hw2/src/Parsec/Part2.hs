module Parsec.Part2 where

import           Control.Applicative ((<|>))
import           Parsec.Parser       (Parser, char, ident, oneOrMore, posInt,
                                      spaces)

type Ident = String

data Atom = N Integer | I Ident
    deriving (Show)

data SExpr = A Atom
           | Comb [SExpr]
           deriving (Show)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseA <|> parseComb)

parseI :: Parser Atom
parseI = I <$> ident

parseN :: Parser Atom
parseN = N <$> posInt

parseAtom :: Parser Atom
parseAtom = parseN <|> parseI

parseA :: Parser SExpr
parseA = A <$> parseAtom

parseComb :: Parser SExpr
parseComb = char '(' *> pure Comb <*> oneOrMore parseSExpr <* char ')'
