module Parsec.LetExpr where

import           Control.Applicative        ((<|>))
import           Control.Monad.Trans.Reader (Reader, reader, runReader)
import           Control.Monad.Trans.State  (State, evalState, state)
import qualified Data.Map.Strict            as Map (Map, empty, insert, lookup)
import           Parsec.Parser              (Parser, char, ident, oneOrMore,
                                             posInt, sepBy, skipWS, string)

type VarName = String

data Var = Var VarName | Const Integer

data Let = Let { letVarName :: VarName
               , vars       :: [Var]
               }

newtype LetExpr = LetExpr [Let]

data OptimizedLet = OptLet { optLetVarName :: VarName
                           , result        :: Integer
                           }

newtype OptimizedLetExpr = OptLetExpr [OptimizedLet]

parseVar :: Parser Var
parseVar = Var <$> ident <|> Const <$> posInt

parseLet :: Parser Let
parseLet =
    skipWS (string "let") >>
    skipWS ident >>= \x ->
    skipWS (char '=') >>
    (parseVar `sepBy` "+") >>= \xs ->
    return $ Let x xs

parseLetExpr :: Parser LetExpr
parseLetExpr = LetExpr <$> oneOrMore parseLet

showListSep :: (Show a) => String -> [a] -> String
showListSep sep xs = foldr1 (\a b -> a ++ sep ++ b) $ show <$> xs

evalVar :: Var -> Reader (Map.Map String Integer) (Maybe Integer)
evalVar (Const n)  = reader . const $ Just n
evalVar (Var name) = reader $ Map.lookup name

evalLet :: Let -> State (Map.Map String Integer) OptimizedLet
evalLet (Let name xs) = state $ \m -> let
    maybeResList = map (flip runReader m . evalVar) xs
    res = case sequence maybeResList of
        Just args -> sum args
        Nothing   -> error "Invalid expression"
    in
    (OptLet name res, Map.insert name res m)

optimize :: LetExpr -> OptimizedLetExpr
optimize (LetExpr lets) = OptLetExpr $ evalState (mapM evalLet lets) Map.empty

parseAndOptLetExpr :: Parser OptimizedLetExpr
parseAndOptLetExpr = optimize <$> parseLetExpr

instance Show LetExpr where
    show (LetExpr lets) = showListSep "\n" lets

instance Show Let where
    show expr = "let " ++ letVarName expr ++ " = " ++ showListSep " + " (vars expr)

instance Show Var where
    show (Var name) = name
    show (Const n)  = show n

instance Show OptimizedLet where
    show (OptLet name value) = "let " ++ name ++ " = " ++ show value

instance Show OptimizedLetExpr where
    show (OptLetExpr optLets) = showListSep "\n" optLets
