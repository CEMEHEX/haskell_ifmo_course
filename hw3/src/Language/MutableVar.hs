module Language.MutableVar
    (
      Command (..)
    , getVars
    , create
    , update
    ) where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Language.Utils             (Error (AlreadyExists, VarNotInScope),
                                             NameToVal, VarName)


newtype Command a = Command { runCmd :: StateT (NameToVal a) (Either Error) () }

getVars :: Command a
        -> NameToVal a
        -> Either Error (NameToVal a)
getVars = execStateT . runCmd

create :: VarName -> a -> Command a
create name value = Command $ do
    m <- gets $ Map.lookup name
    case m of
        Nothing -> modify $ Map.insert name value
        _       -> lift . Left . AlreadyExists $ name

update :: VarName -> a -> Command a
update name value = Command $ do
    m <- gets $ Map.lookup name
    case m of
        Nothing -> lift . Left . VarNotInScope $ name
        _       -> modify $ Map.insert name value