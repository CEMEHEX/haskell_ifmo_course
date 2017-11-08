module Construction.MutableVar
    (
      VarAlteration
    , getVars
    , create
    , update
    ) where

import           Construction.Error         (VariableError (..))
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Data.Text                  as T (Text)


type VarName = T.Text
type NameToVal a = Map.Map VarName a

newtype VarAlteration a = VarAlt
    { runAlt :: StateT (NameToVal a) (Either VariableError) () }

getVars :: VarAlteration a
        -> NameToVal a
        -> Either VariableError (NameToVal a)
getVars = execStateT . runAlt

create :: VarName -> a -> VarAlteration a
create name value = VarAlt $ do
    m <- gets $ Map.lookup name
    case m of
        Nothing -> modify $ Map.insert name value
        _       -> lift . Left . AlreadyExists $ name

update :: VarName -> a -> VarAlteration a
update name value = VarAlt $ do
    m <- gets $ Map.lookup name
    case m of
        Nothing -> lift . Left . NoSuchVar $ name
        _       -> modify $ Map.insert name value
