module Construction.Error
    (
      ArithmError (..)
    , VariableError (..)
    ) where

import qualified Data.Text as T (Text)

data ArithmError = DivByZero
                 | VarNotInScope T.Text
                 deriving (Show, Eq)

data VariableError = NoSuchVar T.Text
                   | AlreadyExists T.Text
                   deriving (Show, Eq)
