{-# LANGUAGE NoImplicitPrelude #-}

module BaseTypeClasses.ChaoticGood.Identity where


import           Prelude (Applicative, Foldable, Functor, Traversable, fmap,
                          foldMap, pure, sequenceA, traverse, ($), (.), (<$>),
                          (<*>))

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f = Identity . f . runIdentity

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f = f . runIdentity

instance Traversable Identity where
    sequenceA (Identity f) = Identity <$> f

    traverse f (Identity a) = Identity <$> f a

-- PROOFS

{- Applicative1: pure id <*> v = v
    pure id <*> Identity a  === Identity id <*> Identity a                      -- definition of pure
                            === Identity $ id a                                 -- definition of <*>
                            === Identity a                                      -- definition of id
-}

{- Applicative2: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    pure (.) <*> Identity a <*> Identity b <*> Identity c
        === Identity (.) <*> Identity a <*> Identity b <*> Identity c           -- definition of pure
        === Identity (a .) <*> Identity b <*> Identity c                        -- definition of <*>
        === Identity (a . b) <*> Identity c                                     -- definition of <*>
        === Identity (a . b $ c)                                                -- definition of <*>
        === Identity (a (b (c)))                                                -- definition of (.)
        === Identity a <*> Identity (b (c))                                     -- definition of <*>
        === Identity a <*> (Identity b <*> Identity c)                          -- definition of <*>
-}
newtype Compose f g a = Compose (f (g a))
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

t1:: (Applicative g, Applicative f) =>
        Identity (f (g a)) -> Compose f g (Identity a)
t1 (Identity a) = Compose (fmap (fmap Identity) a)

t2 :: (Applicative f, Applicative g) =>
        Identity (f (g a)) -> Compose f g (Identity a)
t2 (Identity a) = Compose (fmap (sequenceA . Identity) a)
{- Traversable3: sequenceA . fmap Compose === Compose . fmap sequenceA . sequenceA
    (sequenceA . fmap Compose) (Identity a)
        === sequenceA (fmap Compose (Identity a))                   -- definition of (.)
        === sequenceA (Identity (Compose a))                        -- definition of fmap for Identity
        === fmap Identity (Compose a)                               -- definition of sequenceA for Identity
        === Compose (fmap (fmap Identity) a)                        -- definition of fmap for Compose

    (Compose . fmap sequenceA . sequenceA) (Identity a)
        === Compose (fmap sequenceA (sequenceA (Identity a)))       -- definition of (.)
        === Compose (fmap sequenceA (fmap Identity a))              -- definition of sequenceA for Identity
        === Compose (fmap (sequenceA . Identity) a)                 -- second functor law
        === Compose (fmap (\x -> sequenceA (Identity x)) a)         -- definition of (.)
        === Compose (fmap (\x -> fmap Identity x) a)                -- definition of sequenceA for Identity
        === Compose (fmap (fmap Identity) a)                        -- η - reduction
-}
