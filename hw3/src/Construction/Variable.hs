module Construction.Variable
    (
      VarDecl (..)
    , execVar
    ) where

import           Construction.MutableVar (VarAlteration, create, update)
import           Data.Text               as T (Text)

type VarName = T.Text

data VarDecl a = New VarName a | Upd VarName a
    deriving (Show, Eq)

execVar :: VarDecl a -> VarAlteration a
execVar (New name value) = create name value
execVar (Upd name value) = update name value
