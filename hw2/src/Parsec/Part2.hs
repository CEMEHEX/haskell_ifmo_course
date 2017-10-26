module Parsec.Part2 where

import           Parsec.Parser (Parser)

type Ident = String

data Atom = N Integer | I Ident
    deriving (Show)

data SExpr = A Atom
           | Comb [SExpr]
           deriving (Show)

parseSExpr :: Parser SExpr
parseSExpr = undefined
