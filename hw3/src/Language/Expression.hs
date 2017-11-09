module Language.Expression
    (
      Expr(..)
    , getResult
    ) where

import           Control.Applicative  (ZipList (ZipList))
import           Control.Monad.Reader (ReaderT, ask, lift, local, runReaderT,
                                       when)
import qualified Data.Map.Strict      as Map (insert, lookup)
import           Language.Utils       (Error (DivByZero, VarNotInScope),
                                       Expr (..), NameToVal)

type BinOp a = a -> a -> a
type ErrorCheckers a e = [a -> a -> Either e ()]
type SafeWrapper a e = BinOp a
                    -> Expr a
                    -> Expr a
                    -> ErrorCheckers a e
                    -> ExprEvaluation a

newtype ExprEvaluation a = ExprEvaluation
    { runEvaluation :: ReaderT (NameToVal a) (Either Error) a }

getResult :: (Integral a) => Expr a -> NameToVal a -> Either Error a
getResult = runReaderT . runEvaluation . eval

eval :: (Integral a) => Expr a -> ExprEvaluation a
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

wrapSafe :: (Integral a) => SafeWrapper a Error
wrapSafe op x y checkers = ExprEvaluation $ do
    xVal <- runEvaluation $ eval x
    yVal <- runEvaluation $ eval y
    lift $ runCheckers xVal yVal checkers
    return $ xVal `op` yVal

runCheckers :: a -> a -> ErrorCheckers a e -> Either e ()
runCheckers a b checkers = sequence_ $ ZipList checkers <*> pure a <*> pure b

noCheckers :: ErrorCheckers a e
noCheckers = []

divCheckers :: (Num a, Eq a) => ErrorCheckers a Error
divCheckers = [\_ b -> when (b == 0) $ Left DivByZero]
