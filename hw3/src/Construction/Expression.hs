module Construction.Expression
    (
      Expr(..)
    , getResult
    ) where

import           Construction.Error   (ArithmError (..))
import           Control.Applicative  (ZipList (ZipList))
import           Control.Monad.Reader (ReaderT, ask, lift, local, runReaderT,
                                       when)
import qualified Data.Map.Strict      as Map (Map, insert, lookup)
import qualified Data.Text            as T (Text)

type VarName = T.Text
type BinOp a = a -> a -> a
type NameToVal a = Map.Map VarName a
type ErrorCheckers a e = [a -> a -> Either e ()]
type SafeWrapper a e = BinOp a
                    -> Expr
                    -> Expr
                    -> ErrorCheckers a e
                    -> ExprEvaluation

newtype ExprEvaluation = ExprEvaluation
    { runEvaluation :: ReaderT (NameToVal Integer) (Either ArithmError) Integer }
-- TODO do it in more generic way
data Expr = Lit Integer
          | Var VarName
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let VarName Expr Expr
          deriving (Show, Eq)

getResult :: Expr -> NameToVal Integer -> Either ArithmError Integer
getResult = runReaderT . runEvaluation . eval

eval :: Expr -> ExprEvaluation
eval (Lit x)        = ExprEvaluation $ return x
eval (Var x)        = ExprEvaluation $ do
    m <- ask
    case Map.lookup x m of
        Nothing  -> lift . Left . VarNotInScope $ x
        Just val -> return val
eval (Add x y)      = wrapSafe (+) x y noCheckers
eval (Sub x y)      = wrapSafe (-) x y noCheckers
eval (Mul x y)      = wrapSafe (*) x y noCheckers
eval (Div x y)      = wrapSafe div x y divCheckers
eval (Let name x y) = ExprEvaluation $ do
    xVal <- runEvaluation $ eval x
    local (Map.insert name xVal) (runEvaluation $ eval y)

wrapSafe :: SafeWrapper Integer ArithmError
wrapSafe op x y checkers = ExprEvaluation $ do
    xVal <- runEvaluation $ eval x
    yVal <- runEvaluation $ eval y
    lift $ runCheckers xVal yVal checkers
    return $ xVal `op` yVal

runCheckers :: a -> a -> ErrorCheckers a e -> Either e ()
runCheckers a b checkers = sequence_ $ ZipList checkers <*> pure a <*> pure b

noCheckers :: ErrorCheckers a e
noCheckers = []

divCheckers :: ErrorCheckers Integer ArithmError
divCheckers = [\_ b -> when (b == 0) $ Left DivByZero]
