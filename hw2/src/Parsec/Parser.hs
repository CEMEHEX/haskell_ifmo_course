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

-- PROOFS:

{- fmap id === id (OR fmap id p === p)
    fmap id p   === Parser $ fmap (first id) . runParser p                      -- definition of fmap
                === Parser $ \s -> fmap (first id) (runParser p s)              -- definition of (.)

--  first id (a, b) === (a, b) => first id === id                               -- definition of first

                === Parser $ \s -> fmap id (runParser p s)                      -- first id === id
                === Parser $ \s -> runParser p s                                -- first fmap law
                === Parser $ runParser p                                        -- η - reduction
                === p                                                           -- definition of Parser
-}

{- pure id <*> p = p
    pure id <*> (Parser p)  === Parser (\s -> Just (id, s)) <*> (Parser p)      -- definition of pure
                            === Parser $ \s ->
                                Just (id, s)  >>= \(f, s') ->
                                p s' >>= \(v, s'') ->
                                return (f v, s'')                               -- definition of <*>
                            === Parser $ \s ->
                                p s >>= \(v, s'') ->
                                return (id v, s'')                              -- definition of >>=
                            === Parser $ \s ->
                                ps >>= \(v, s'') -> return (v, s'')             -- definition of id
                            === Parser $ \s -> p s >>= return                   -- η - reduction
                            === Parser $ \s -> p s                              -- first monad law
                            === Parser p                                        -- η - reduction
-}

{- empty <|> u === u <|> empty === u
    empty <|> Parser p  === Parser $ (const Nothing) <|> Parser p                 -- definition of empty
                        === Parser $ \s -> (const Nothing) s <|> p s            -- definition of <|>
                        === Parser $ \s -> Nogthing <|> p s                     -- β - reduction
                        === Parser $ \s -> p s                                  -- definition of <|> for Maybe
                        === Parser p                                            -- η - reduction

    Parser p <|> empty === Parser $ Parser p <|> const Nothing                  -- similarly...
-}

{- Parser p >>= return === Parser p
    Parser p >>= return === Parser $ \s ->
                                p s >>= \(a, s') ->
                                runParser (return a) s'                         -- definition of >>=
                        === Parser $ \s ->
                                p s >>= \(a, s') ->
                                runParser (Parser $ \s -> Just (a, s)) s'       -- definition of return(pure)
                        === Parser $ \s ->
                                p s >>= \(a, s') ->
                                Just (a, s')                                    -- definition of runParser and β - reduction
                        === Parser $ \s ->
                                p s >>= \(a, s') -> return (a, s')              -- definition of return for Maybe
                        === Parser $ \s -> p s >>= return                       -- η - reduction
                        === Parser $ \s -> p s                                  -- first monad law
                        === Parser p                                            -- η - reduction
-}
