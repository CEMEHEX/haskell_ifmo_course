module Parsec.Part1 where

import           Control.Applicative ((<|>))
import           Parsec.Parser       (Parser, char, ignored, posInt, space,
                                      upper)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser()
abParser_ = ignored abParser

intPair :: Parser [Integer]
intPair = pairListBuilder <$> posInt <* space <*> posInt
    where
        pairListBuilder a b = [a, b]

intOrUpperCase :: Parser ()
intOrUpperCase = ignored posInt <|> ignored upper
