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

-- PROOFS
{- Traversable1: t . sequenceA === sequenceA . fmap t for every applicative transformation t
    (t . sequenceA) (Const a)   === t (pure (Const a))                          -- definition of (.)
                                === pure (Const a)                              -- def of applicative transformation

    (sequenceA . fmap t) (Const a) === sequenceA (fmap t (Const a))             -- definition of (.)
                                   === sequenceA (Const a)                      -- definition of fmap
                                   === pure (Const a)                           -- definition of sequenceA
-}

{- Traversable2: sequenceA . fmap Identity === Identity
    (sequenceA . fmap Identity) (Const a) === sequenceA (fmap Identity Const a) -- definition of (.)
                                          === sequenceA (Const a)               -- definition of fmap
                                          === pure (Const a)                    -- definition of sequenceA
                                          === Identity (Const a)                -- definition of pure for Identity
-}

{- Functor1: fmap id === id
    fmap id (Const a) === Const a               -- definition of fmap
-}

{- Functor2: fmap f . fmap g === fmap (f . g)
    \x -> fmap f (fmap g x) === fmap g x        -- definition of fmap
                            === x               -- definition of fmap

    \x -> fmap (f . g) x    === x               -- definition of fmap
-}
