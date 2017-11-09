module Language.Utils
    (
      VarName
    , NameToVal
    , Expr(..)
    , Error(..)
    ) where

import qualified Data.Map.Strict as Map (Map)
import           Data.Text       as T (Text)

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

data Error = DivByZero
           | VarNotInScope T.Text
           | AlreadyExists T.Text
           deriving (Show, Eq)
