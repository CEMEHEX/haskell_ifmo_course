{-# LANGUAGE TupleSections #-}

module Parsec.Parser where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       ((>=>))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace,
                                      isUpper)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Instances -------------------------------------------------------------------
instance Functor Parser where
    fmap f p = Parser $ fmap (first f) . runParser p
        where
            first :: (a -> c) -> (a, b) -> (c, b)
            first g (a, b) = (g a, b)

instance Applicative Parser where
    pure a = Parser $ Just . (a,)

    (Parser pf) <*> (Parser pv) = Parser $
        pf    >=> \(f, s') ->
        pv s' >>= \(v, s'') ->
        return (f v, s'')

instance Alternative Parser where
    empty = Parser $ const Nothing

    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
    -- :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $
        p >=> \(a, s') ->
        runParser (f a) s'


-- Functions ------------------------------------------------------------------
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

sepBy :: Parser a -> String -> Parser [a]
sepBy p sep = skipWS $ pure (:) <*> p <*> next
    where
        next =
            skipWS (string sep) *>
            skipWS (pure (:)) <*>
            p <*>
            next <|>
            pure []

skipWS :: Parser a -> Parser a
skipWS = (spaces *>)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
        f []            = Nothing
        f (x:xs)
            | p x       = Just (x, xs)
            | otherwise = Nothing

posInt :: Parser Integer
posInt = Parser f
    where
        f xs
            | null ns            = Nothing
            | otherwise          = Just (read ns, rest)
                where (ns, rest) = span isDigit xs

string :: String -> Parser String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

char :: Char -> Parser Char
char c = satisfy (== c)

upper :: Parser Char
upper = satisfy isUpper

space :: Parser Char
space = satisfy $ \c -> isSpace c && c /= '\n'

ignored :: Parser a -> Parser ()
ignored = (*> pure ())

spaces :: Parser String
spaces = zeroOrMore space

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)
