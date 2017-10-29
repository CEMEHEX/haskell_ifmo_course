module Monstupar.Simple where

import           Control.Applicative ((<|>))
import           Monstupar.Parser    (Monstupar, char, ignored, posInt, space,
                                      upper)

abParser :: Monstupar Char (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Monstupar Char ()
abParser_ = ignored abParser

intPair :: Monstupar Char [Integer]
intPair = pairListBuilder <$> posInt <* space <*> posInt
    where
        pairListBuilder a b = [a, b]

intOrUpperCase :: Monstupar Char ()
intOrUpperCase = ignored posInt <|> ignored upper
