module Language.MutableVar
    (
      Command (..)
    , create
    , update
    , delete
    ) where

import           Control.Monad.State.Strict (gets, lift, modify)

import qualified Data.Map.Strict            as Map (delete, insert, lookup)
import           Language.Utils             (Command (..), RuntimeError (AlreadyExists, VarNotInScope),
                                             VarName, except)




-- TODO replace case of construction with maybe
create :: VarName -> a -> Command a ()
create name value = Command $ do
    m <- gets $ Map.lookup name
    case m of
        Nothing -> modify $ Map.insert name value
        _       -> lift . except . Left . AlreadyExists $ name

update :: VarName -> a -> Command a ()
update name value = Command $ do
    m <- gets $ Map.lookup name
    case m of
        Nothing -> lift . except . Left . VarNotInScope $ name
        _       -> modify $ Map.insert name value

delete :: VarName -> Command a ()
delete = Command . modify . Map.delete
