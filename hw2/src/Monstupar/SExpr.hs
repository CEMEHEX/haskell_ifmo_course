module Monstupar.SExpr where

import           Control.Applicative ((<|>))
import           Monstupar.Parser    (Monstupar, char, ident, oneOrMore, posInt,
                                      skipWS)

type Ident = String

data Atom = N Integer | I Ident
    deriving (Show, Eq)

data SExpr = A Atom
           | Comb [SExpr]
           deriving (Show, Eq)

parseSExpr :: Monstupar Char SExpr
parseSExpr = skipWS $ parseA <|> parseComb

parseI :: Monstupar Char Atom
parseI = I <$> ident

parseN :: Monstupar Char Atom
parseN = N <$> posInt

parseAtom :: Monstupar Char Atom
parseAtom = parseN <|> parseI

parseA :: Monstupar Char SExpr
parseA = A <$> parseAtom

parseComb :: Monstupar Char SExpr
parseComb = char '(' *> skipWS (pure Comb) <*> oneOrMore parseSExpr <* skipWS (char ')')
