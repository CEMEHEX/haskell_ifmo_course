{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Monstupar.Parser where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       ((>=>))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace,
                                      isUpper)

data ParseError = ParseError
              deriving (Show, Eq)

newtype Monstupar s a = Parser { runParser :: [s] -> Either ParseError (a, [s]) }

-- Instances -------------------------------------------------------------------
instance Functor (Monstupar s) where
    fmap f p = Parser $ fmap (first f) . runParser p
        where
            first :: (a -> c) -> (a, b) -> (c, b)
            first g (a, b) = (g a, b)

instance Applicative (Monstupar s) where
    pure a = Parser $ Right . (a,)

    (Parser pf) <*> (Parser pv) = Parser $
        pf    >=> \(f, s') ->
        pv s' >>= \(v, s'') ->
        return (f v, s'')

instance Alternative (Monstupar s) where
    empty = Parser $ const (Left ParseError)

    (Parser p1) <|> (Parser p2) = Parser $ \s -> case p1 s of
        Left _  -> p2 s
        Right a -> Right a

instance Monad (Monstupar s) where
    (Parser p) >>= f = Parser $
        p >=> \(a, s') ->
        runParser (f a) s'


-- Functions ------------------------------------------------------------------
zeroOrMore :: Monstupar s a -> Monstupar s [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

oneOrMore :: Monstupar s a -> Monstupar s [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

sepBy :: Monstupar Char a -> String -> Monstupar Char [a]
sepBy p sep = skipWS $ pure (:) <*> p <*> next
    where
        next =
            skipWS (string sep) *>
            skipWS (pure (:)) <*>
            p <*>
            next <|>
            pure []

skipWS :: Monstupar Char a -> Monstupar Char a
skipWS = (spaces *>)

satisfy :: (s -> Bool) -> Monstupar s s
satisfy p = Parser f
    where
        f []            = Left ParseError
        f (x:xs)
            | p x       = Right (x, xs)
            | otherwise = Left ParseError

posInt :: Monstupar Char Integer
posInt = Parser f
    where
        f xs
            | null ns            = Left ParseError
            | otherwise          = Right (read ns, rest)
                where (ns, rest) = span isDigit xs

string :: String -> Monstupar Char String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

char :: Char -> Monstupar Char Char
char c = satisfy (== c)

upper :: Monstupar Char Char
upper = satisfy isUpper

space :: Monstupar Char Char
space = satisfy $ \c -> isSpace c && c /= '\n'

ignored :: Monstupar s a -> Monstupar s ()
ignored = (*> pure ())

spaces :: Monstupar Char String
spaces = zeroOrMore space

ident :: Monstupar Char String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)
