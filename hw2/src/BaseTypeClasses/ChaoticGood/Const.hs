{-# LANGUAGE NoImplicitPrelude #-}

module BaseTypeClasses.ChaoticGood.Const where

import           Data.Monoid ((<>))
import           Prelude     (Applicative, Foldable, Functor, Monoid,
                              Traversable, fmap, foldMap, mempty, pure,
                              sequenceA, ($), (<*>))

newtype Const a b = Const a

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance (Monoid m) => Applicative (Const m) where
    pure _ = Const mempty

    (Const m1) <*> (Const m2) = Const $ m1 <> m2

instance Foldable (Const m) where
    foldMap _ _ = mempty

instance Traversable (Const a) where
    sequenceA (Const a) = pure $ Const a
