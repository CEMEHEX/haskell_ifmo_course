module OptionalMonads.Arithm where

type Result = Either ArithmeticError Integer

type BinOp = Integer -> Integer -> Integer

type ErrorChecker = Integer -> Integer -> Bool

data Expr = Const Integer
          | Addition Expr Expr
          | Subtraction Expr Expr
          | Multipliation Expr Expr
          | Division Expr Expr
          | Power Expr Expr
          deriving (Eq)

data ArithmeticError = DivByZero
                     | NegativeExp
                     | NoError
                     deriving (Eq)

eval :: Expr -> Result
eval (Const a)           = return a
eval (Addition x y)      = wrapSafe (+) noError NoError x y
eval (Subtraction x y)   = wrapSafe (-) noError NoError x y
eval (Multipliation x y) = wrapSafe (*) noError NoError x y
eval (Division x y)      = wrapSafe div checker DivByZero x y
  where
      checker _ b = b == 0
eval (Power x y)         = wrapSafe (^) checker NegativeExp x y
  where
      checker _ b = b < 0

wrapSafe :: BinOp
         -> ErrorChecker
         -> ArithmeticError
         -> Expr
         -> Expr
         -> Result
wrapSafe op errorChecker err x y =
    eval x >>= \a ->
    eval y >>= \b ->
    if errorChecker a b
        then Left err
        else return $ a `op` b

noError :: ErrorChecker
noError _ _ = False

instance Show ArithmeticError where
    show DivByZero   = "*** ArithmeticError: division by zero"
    show NegativeExp = "*** ArithmeticError: negative exponent"
    show NoError     = "No error"

instance Show Expr where
    show (Const a)           = show a
    show (Addition x y)      = showOp "+" x y
    show (Subtraction x y)   = showOp "-" x y
    show (Multipliation x y) = showOp "*" x y
    show (Division x y)      = showOp "/" x y
    show (Power x y)         = showOp "^" x y

wrapExpr :: Expr -> String
wrapExpr (Const a) = show a
wrapExpr e         = "(" ++ show e ++ ")"

showOp :: String -> Expr -> Expr -> String
showOp op x y = wrapExpr x ++ " " ++ op ++ " " ++ wrapExpr y
