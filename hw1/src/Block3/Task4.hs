module Block3.Task4 where

data Nat = Z | S Nat deriving Show

negativeNatError :: e
negativeNatError = error "Nat can't be negative"

divisionByZeroError :: e
divisionByZeroError = error "division by zero"

instance Eq Nat where
    Z == Z = True
    (S a) == (S b) = a == b
    _ == _ = False

instance Ord Nat where
    Z <= _ = True
    (S a) <= (S b) = a <= b
    _ <= Z = False

instance Num Nat where
    Z + n = n
    n + Z = n
    (S a) + b = S (a + b)

    n - Z = n
    Z - _ = negativeNatError
    (S a) - (S b) = a - b

    Z * _ = Z
    _ * Z = Z
    a * (S b) = a + (a * b)

    abs = id

    signum Z = 0
    signum _ = 1

    fromInteger n | n < 0 = negativeNatError
                  | otherwise = iterate S Z !! fromIntegral n

fromNat :: (Num a) => Nat -> a
fromNat Z     = 0
fromNat (S n) = fromNat n + 1

isEven :: Nat -> Bool
isEven Z     = True
isEven (S n) = not $ isEven n

isOdd :: Nat -> Bool
isOdd = not . isEven

natMod :: Nat -> Nat -> Nat
natMod _ Z = divisionByZeroError
natMod a b = until ( < b) (subtract b) a

natDiv :: Nat -> Nat -> Nat
natDiv _ Z = divisionByZeroError
natDiv a b = until (\n -> (n + 1) * b > a) (+ 1) 0

natGcd :: Nat -> Nat -> Nat
natGcd a 0 = a
natGcd a b = natGcd b $ natMod a b
