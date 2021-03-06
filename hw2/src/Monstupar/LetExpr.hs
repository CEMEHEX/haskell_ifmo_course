module Monstupar.LetExpr where

import           Control.Applicative        ((<|>))
import           Control.Monad.Trans.Reader (Reader, reader, runReader)
import           Control.Monad.Trans.State  (State, evalState, state)
import qualified Data.Map.Strict            as Map (Map, empty, insert, lookup)
import           Monstupar.Parser           (Monstupar, char, ident, posInt,
                                             sepBy, skipWS, spaces, string)

type VarName = String

data Var = Var VarName | Const Integer
    deriving (Eq)

data Let = Let { letVarName :: VarName
               , vars       :: [Var]
               }
               deriving (Eq)

newtype LetExpr = LetExpr [Let]
    deriving(Eq)

data OptimizedLet = OptLet { optLetVarName :: VarName
                           , result        :: Integer
                           }
                           deriving (Eq)

newtype OptimizedLetExpr = OptLetExpr [OptimizedLet]
    deriving (Eq)

parseVar :: Monstupar Char Var
parseVar = Var <$> ident <|> Const <$> posInt

parseLet :: Monstupar Char Let
parseLet =
    skipWS (string "let") >>
    skipWS ident >>= \x ->
    skipWS (char '=') >>
    (parseVar `sepBy` "+") >>= \xs ->
    spaces >>
    return (Let x xs)

parseLetExpr :: Monstupar Char LetExpr
parseLetExpr = LetExpr <$> (parseLet `sepBy` "\n")

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

parseAndOptLetExpr :: Monstupar Char OptimizedLetExpr
parseAndOptLetExpr = optimize <$> parseLetExpr

showListSep :: (Show a) => String -> [a] -> String
showListSep sep xs = foldr1 (\a b -> a ++ sep ++ b) $ show <$> xs

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
