module Language.Utils
    (
      Code
    , Command (..)
    , Expr(..)
    , IOAction (..)
    , NameToVal
    , Program
    , VarName
    , RuntimeError(..)
    , Statement (..)
    , except
    , mkExceptIO
    , mkIOAction
    , wrapParserOutput
    ) where

import           Data.Functor.Identity      (runIdentity)
import qualified Data.Map.Strict            as Map (Map)
import           Data.Text                  as T (Text, unpack)
import           Data.Void                  (Void)

import           Control.Monad.Except       (Except, ExceptT (..), runExceptT)
import           Control.Monad.State.Strict (StateT (..), runStateT)

import           Text.Megaparsec.Error      (ParseError, parseErrorPretty)
import           Text.Megaparsec.Stream     (Token)

type VarName = T.Text
type Code = T.Text
type NameToVal a = Map.Map VarName a

data Expr a
    = Lit a
    | Var VarName
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    | Div (Expr a) (Expr a)
    | Let VarName (Expr a) (Expr a)
    deriving (Eq)

data RuntimeError
    = DivByZero
    | VarNotInScope VarName
    | AlreadyExists VarName
    | NoParse String
    deriving (Eq)

newtype Command a b =
    Command { runCmd :: StateT (NameToVal a) (Except RuntimeError) b }

newtype IOAction a b = IOAction
    { runIOAction :: StateT (NameToVal a) (ExceptT RuntimeError IO) b }

type Program a = [Statement a]

data Statement a
   = New VarName (Expr a)
   | Upd VarName (Expr a)
   | Out (Expr a)
   | In VarName
   | For VarName (Expr a) (Expr a) (Program a)
   deriving (Show, Eq)

wrapParserOutput :: Either (ParseError (Token Text) Void) a
                 -> Either RuntimeError a
wrapParserOutput (Right a) = Right a
wrapParserOutput (Left e)  = Left . NoParse . parseErrorPretty $ e

mkExceptIO :: Except e a -> ExceptT e IO a
mkExceptIO = ExceptT . return . runIdentity . runExceptT

mkIOAction :: Command a b -> IOAction a b
mkIOAction cmd = IOAction . StateT $ \s -> mkExceptIO ((runStateT . runCmd) cmd s)

except :: Either e a -> Except e a
except  = ExceptT . return

instance Show RuntimeError where
    show DivByZero            = "Runtime error: division by zero"
    show (VarNotInScope name) = "Variable not in scope: " ++ T.unpack name
    show (AlreadyExists name) = "Variable already exists: " ++ T.unpack name
    show (NoParse msg)        = msg

instance (Show a) => Show (Expr a) where
    show (Lit a)    = show a
    show (Var name) = T.unpack name
    show (Add x y)  = showOp "+" x y
    show (Sub x y)  = showOp "-" x y
    show (Mul x y)  = showOp "*" x y
    show (Div x y)  = showOp "/" x y
    show (Let name x y) =
        "(let " ++ T.unpack name ++ " = " ++ show x ++ " in " ++ show y ++ ")"


wrapExpr :: (Show a) => Expr a -> String
wrapExpr expr@(Lit _)   = show expr
wrapExpr expr@(Var _)   = show expr
wrapExpr expr@(Mul _ _) = show expr
wrapExpr expr           = "(" ++ show expr ++ ")"

showOp :: (Show a) => String -> Expr a -> Expr a -> String
showOp op x y = wrapExpr x ++ " " ++ op ++ " " ++ wrapExpr y
