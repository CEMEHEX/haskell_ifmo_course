{-# LANGUAGE NoImplicitPrelude #-}

module BaseTypeClasses.ChaoticGood where

import           Data.Monoid ((<>))
import           Prelude     (Applicative, Foldable, Functor, Monoid,
                              Traversable, fmap, foldMap, foldr, mempty, pure,
                              sequenceA, traverse, ($), (.), (<$>), (<*>))

-- Declarations ----------------------------------------------------------------

newtype Identity a = Identity { runIdentity :: a }

data Either e a = Left e | Right a

data Tree a = Leaf | Node (Tree a) (Tree a) a

newtype Const a b = Const a

data Pair a b = Pair a b

-- Identity --------------------------------------------------------------------

instance Functor Identity where
    fmap f = Identity . f . runIdentity

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f = f . runIdentity

instance Traversable Identity where
    sequenceA (Identity f) = Identity <$> f

-- Either ----------------------------------------------------------------------

instance Functor (Either e) where
    fmap _ (Left e)  = Left e
    fmap f (Right a) = Right $ f a

instance Applicative (Either e) where
    pure = Right

    (Left e) <*> _ = Left e
    _ <*> (Left e) = Left e
    (Right f) <*> (Right a) = Right $ f a

instance Foldable (Either e) where
    foldMap _ (Left _)  = mempty
    foldMap f (Right a) = f a

instance Traversable (Either e) where
    sequenceA (Left e)  = pure $ Left e
    sequenceA (Right f) = Right <$> f

-- Tree ------------------------------------------------------------------------

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node l r a) = Node (fmap f l) (fmap f r) (f a)

instance Applicative Tree where
    pure = Node Leaf Leaf

    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node l1 r1 f) <*> (Node l2 r2 a) = Node (l1 <*> l2) (r1 <*> r2) (f a)

instance Foldable Tree where
    foldr _ z Leaf         = z
    foldr f z (Node l r a) = foldr f (f a $ foldr f z r) l

instance Traversable Tree where
    sequenceA Leaf         = pure Leaf
    sequenceA (Node l r f) = Node <$> sequenceA l <*> sequenceA r <*> f

    traverse _ Leaf         = pure Leaf
    traverse f (Node l r a) = Node <$> traverse f l <*> traverse f r <*> f a

-- Const -----------------------------------------------------------------------

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance (Monoid m) => Applicative (Const m) where
    pure _ = Const mempty

    (Const m1) <*> (Const m2) = Const $ m1 <> m2

instance Foldable (Const m) where
    foldMap _ _ = mempty

instance Traversable (Const a) where
    sequenceA (Const a) = pure $ Const a

-- Pair ------------------------------------------------------------------------

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a $ f b

instance (Monoid m) => Applicative (Pair m) where
    pure = Pair mempty

    (Pair m1 f) <*> (Pair m2 a) = Pair (m1 <> m2) (f a)

instance Foldable (Pair a) where
    foldMap f (Pair _ a) = f a

instance Traversable (Pair a) where
    sequenceA (Pair a f) = Pair a <$> f
