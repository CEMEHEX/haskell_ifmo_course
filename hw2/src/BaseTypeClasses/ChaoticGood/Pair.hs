{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module BaseTypeClasses.ChaoticGood.Pair where

import           Data.Monoid ((<>))
import           Prelude     (Monoid, id, mempty)

-- Instances -------------------------------------------------------------------

instance Functor ((,) a) where
    fmap f (a, b) = (a, f b)

instance (Monoid m) => Applicative ((,) m) where
    pure = (mempty,)

    (m1, f) <*> (m2, a) = (m1 <> m2, f a)

instance Foldable ((,) a) where
    foldMap f (_, a) = f a

instance Traversable ((,) a) where
    sequenceA (a, f) = (a, ) <$> f

-- Class declarations ----------------------------------------------------------

class Functor f where
    fmap :: (a -> b) -> f a -> f b

infixl 4 <$>
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

class Functor f => Applicative f where
    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

class Foldable t where
    foldMap :: (Monoid m) => (a -> m) -> t a -> m

    fold :: (Monoid m) => t m -> m
    fold = foldMap id

class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a)

-- PROOFS
{- Foldable1: fold ≡ foldMap id
    fold ≡ foldMap id                      -- definition of fold
-}

{- Foldable2: foldMap f ≡ fold . fmap f
      fold . fmap f === foldMap id . fmap f                                         -- definition of fold
                    === foldMap id . (\(a, b) -> (a, f b))                          -- definition of fmap
                    === \(a, b) -> foldMap id ((\(a', b') -> (a', f b')) (a, b))    -- definition of (.)
                    === \(a, b) -> foldMap id (a, f b)                              -- β - reduction
                    === \(a, b) -> id (f b)                                         -- definition of foldMap
                    === \(a, b) -> f b

    \(a, b) -> foldMap f (a, b) === \(a, b) -> f b                                  -- definition of foldMap
-}
