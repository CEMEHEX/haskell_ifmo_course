{-# LANGUAGE NoImplicitPrelude #-}

module BaseTypeClasses.ChaoticGood.Pair where

import           Data.Monoid ((<>))
import           Prelude     (Applicative, Foldable, Functor, Monoid,
                              Traversable, fmap, foldMap, mempty, pure,
                              sequenceA, ($), (<$>), (<*>))

data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a $ f b

instance (Monoid m) => Applicative (Pair m) where
    pure = Pair mempty

    (Pair m1 f) <*> (Pair m2 a) = Pair (m1 <> m2) (f a)

instance Foldable (Pair a) where
    foldMap f (Pair _ a) = f a

instance Traversable (Pair a) where
    sequenceA (Pair a f) = Pair a <$> f
