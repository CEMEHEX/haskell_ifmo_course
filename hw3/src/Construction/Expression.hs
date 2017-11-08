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
                    -> Expr a
                    -> Expr a
                    -> ErrorCheckers a e
                    -> ExprEvaluation a

newtype ExprEvaluation a = ExprEvaluation
    { runEvaluation :: ReaderT (NameToVal a) (Either ArithmError) a }

data Expr a = Lit a
          | Var VarName
          | Add (Expr a) (Expr a)
          | Sub (Expr a) (Expr a)
          | Mul (Expr a) (Expr a)
          | Div (Expr a) (Expr a)
          | Let VarName (Expr a) (Expr a)
          deriving (Show, Eq)

getResult :: (Integral a) => Expr a -> NameToVal a -> Either ArithmError a
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

wrapSafe :: (Integral a) => SafeWrapper a ArithmError
wrapSafe op x y checkers = ExprEvaluation $ do
    xVal <- runEvaluation $ eval x
    yVal <- runEvaluation $ eval y
    lift $ runCheckers xVal yVal checkers
    return $ xVal `op` yVal

runCheckers :: a -> a -> ErrorCheckers a e -> Either e ()
runCheckers a b checkers = sequence_ $ ZipList checkers <*> pure a <*> pure b

noCheckers :: ErrorCheckers a e
noCheckers = []

divCheckers :: (Num a, Eq a) => ErrorCheckers a ArithmError
divCheckers = [\_ b -> when (b == 0) $ Left DivByZero]
