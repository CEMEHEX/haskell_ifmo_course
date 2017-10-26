module Block5.Task2 where

import           Data.Semigroup (Semigroup, (<>))

data NonEmpty a = a :| [a] deriving (Show, Eq)

newtype Identity a = Identity { runIdentity :: a } deriving (Show, Eq)

newtype Name = Name String deriving (Show, Eq)

newtype Endo a = Endo { getEndo :: a -> a }

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup (NonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| (xs ++ (y:ys))

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty

    mappend (Identity a) (Identity b) = Identity $ mappend a b

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid Name where
    mempty = Name ""

    mappend (Name "") name    = name
    mappend name (Name "")    = name
    mappend (Name a) (Name b) = Name $ a ++ "." ++ b

instance Semigroup Name where
    (<>) = mappend

instance Monoid (Endo a) where
    mempty = Endo { getEndo = id }

    mappend e1 e2 = Endo {getEndo = getEndo e1. getEndo e2 }

instance Semigroup (Endo a) where
    (<>) = mappend

instance Semigroup b => Semigroup (Arrow a b) where
    arr1 <> arr2 = Arrow { getArrow = resArrow }
      where
        f1 = getArrow arr1
        f2 = getArrow arr2
        resArrow a = f1 a <> f2 a

instance Monoid    b => Monoid    (Arrow a b) where
    mempty = Arrow { getArrow = const mempty }

    mappend arr1 arr2 = Arrow { getArrow = resArrow }
      where
        f1 = getArrow arr1
        f2 = getArrow arr2
        resArrow a = mappend (f1 a) (f2 a)
