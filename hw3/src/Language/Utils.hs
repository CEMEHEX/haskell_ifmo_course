module Language.Utils
    (
      Command (..)
    , IOAction (..)
    , Program
    , VarName
    , NameToVal
    , Expr(..)
    , RuntimeError(..)
    , Statement (..)
    , except
    , mkIOAction
    , mkExceptIO
    , wrapParserOutput
    ) where

import           Data.Functor.Identity      (runIdentity)
import qualified Data.Map.Strict            as Map (Map)
import           Data.Text                  as T (Text)

import           Control.Monad.Except       (Except, ExceptT (..), runExceptT)
import           Control.Monad.State.Strict (StateT (..), runStateT)
import           Data.Void                  (Void)
import           Text.Megaparsec.Error      (ParseError, parseErrorPretty)
import           Text.Megaparsec.Stream     (Token)

type VarName = T.Text
type NameToVal a = Map.Map VarName a

data Expr a = Lit a
          | Var VarName
          | Add (Expr a) (Expr a)
          | Sub (Expr a) (Expr a)
          | Mul (Expr a) (Expr a)
          | Div (Expr a) (Expr a)
          | Let VarName (Expr a) (Expr a)
          deriving (Show, Eq)

data RuntimeError
    = DivByZero
    | VarNotInScope VarName
    | AlreadyExists VarName
    | NoParse String
    deriving (Show, Eq)

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
   | For VarName a a (Program a)
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
