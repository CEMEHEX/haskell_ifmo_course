module Language.MutableVar
    (
      Command (..)
    , create
    , delete
    , update
    ) where

import           Control.Monad.State.Strict (gets, lift, modify)
import qualified Data.Map.Strict            as Map (delete, insert, lookup)
import           Data.Maybe                 (maybe)

import           Language.Utils             (Command (..), RuntimeError (AlreadyExists, VarNotInScope),
                                             VarName, except)


create :: VarName -> a -> Command a ()
create name value = Command $ do
    m <- gets $ Map.lookup name
    maybe (modify $ Map.insert name value)
        (const . lift . except . Left . AlreadyExists $ name) m

update :: VarName -> a -> Command a ()
update name value = Command $ do
    m <- gets $ Map.lookup name
    maybe (lift . except . Left . VarNotInScope $ name)
        (const . modify $ Map.insert name value) m

delete :: VarName -> Command a ()
delete = Command . modify . Map.delete
